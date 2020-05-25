package lms.core


import scala.collection.{immutable, mutable}

object Backend {

  // TODO: add type info and location info

  // Expressions are first-class entities in a program
  // that can be manipulated freely. An expression is
  // either a constant or a symbolic reference.
  abstract class Exp extends Def

  case class Sym(n: Int) extends Exp {
    override def toString = s"x$n"
  }
  case class Const(x: Any) extends Exp {
    override def toString = if (x != null) x.toString else "null"
  }
  implicit val orderingExp: Ordering[Exp] = Ordering.by(e => e match {
    case Const(s) => -s.##.abs
    case Sym(n) => n
  })
  implicit val orderingSym: Ordering[Sym] = Ordering.by(_.n)

  val nextName = new mutable.HashMap[String,Int]()
  final def unstableEffect(pref: String) = { // Const(pref + nextName.updateWith(pref) { v => v.map(_ + 1) orElse Some(0) } get) // 2.13!!
    val i = nextName.getOrElseUpdate(pref, 0)
    try { Const(pref + i) } finally { nextName(pref) = i + 1 }
  }


  final def UNSAFE = unstableEffect("UNSAFE")
  val STORE = Const("STORE")
  // A list of effect keys (mutable vars, ...) and
  // dependencies (previous writes, ...)
  //
  // TOOD: this can be made more fine-grained, i.e.,
  // distinguish may/must and read/write effects,
  // soft dependencies, ...
  case class EffectSummary(sdeps: Set[Sym], hdeps: Set[Sym], rkeys: Set[Exp], wkeys: Set[Exp]) {
    lazy val keys = rkeys ++ wkeys
    lazy val deps = sdeps ++ hdeps
    // the string representation of EffectSummary is "[key : sdeps | hdeps]" where write-keys have *.
    def repr(f: Exp => String) = {
      val keyString = (if (rkeys.nonEmpty) rkeys.toSeq.sorted.map(f).mkString(" ") + " " else "") +
        (if (wkeys.nonEmpty) wkeys.toSeq.sorted.map(f(_) + "*").mkString(" ") else "")
      val depString = (if (sdeps.nonEmpty) sdeps.toSeq.sorted.map(f).mkString(", ") else "_") + " | " +
        (if (hdeps.nonEmpty) hdeps.toSeq.sorted.map(f).mkString(", ") else "_")
      "[" + keyString + ": " + depString + "]"
    }
    override def toString = if (!isPure) {
      repr(_.toString)
    } else ""

    def isEmpty = isPure
    lazy val isPure = sdeps.isEmpty && hdeps.isEmpty && rkeys.isEmpty && wkeys.isEmpty
    lazy val hasSimpleEffect = wkeys.exists(key => key.isInstanceOf[Const] && key != STORE)

    def filter(f: Sym => Boolean) = {
      val nf = (e: Exp) => e match {
        case s: Sym => f(s)
        case _ => true
      }
      EffectSummary(sdeps.filter(nf), hdeps.filter(nf), rkeys.filter(nf), wkeys.filter(nf))
    }
  }

  final val emptySummary = EffectSummary(Set(), Set(), Set(), Set())
  final def softSummary(s: Sym) = EffectSummary(Set(s), Set(), Set(), Set())
  final def hardSummary(s: Sym) = EffectSummary(Set(), Set(s), Set(), Set())
  final def writeSummary(s: Exp) = EffectSummary(Set(), Set(), Set(), Set(s))

  // A node in a computation graph links a symbol with
  // its definition, consisting of an operator and its
  // arguments, as well as an effect summary.

  case class Node(n: Sym, op: String, rhs: List[Def], eff: EffectSummary) {
    override def toString = s"$n = ($op ${rhs.mkString(" ")})  $eff"
  }

  /* YYY TODO
    + Node should have a separate field for effect dependencies
      in addition to effect keys. Currently they are lumped
      at the end of rhs (wrapped in Eff(...)) which does not
      seem ideal.
    + Block should separate out input effects, too
    - Should (can?) we get rid of type Def? what prevents
      us from allowing any value as part of rhs
      (guess: typeclass to/from conversion for FrontEnd)
  */

  // A definition is part of the right-hand side of a
  // node definition.
  abstract class Def

  // inputs, result, effect input, effect output summary
  case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary) extends Def {
    def bound = ein::in
    def used = res match {
      case res: Sym => eff.hdeps + res
      case _ => eff.hdeps
    }
    def deps = used ++ eff.sdeps
    // NOTE/TODO: want to mask out purely local effects
    // STORE pure??
    def isPure = eff.hdeps == Set(ein)
  }


  // where should this go?
  def boundSyms(x: Node): Seq[Sym] = blocks(x) flatMap (_.bound)

  def blocks(x: Node): List[Block] =
    x.rhs.collect { case a @ Block(_,_,_,_) => a }

  def directSyms(x: Node): List[Sym] =
    x.rhs.flatMap {
      case s: Sym => List(s)
      case _ => Nil
    }

  // TODO: remove filter syms
  def filterSym(x: List[Exp]): Set[Sym] = x collect { case s: Sym => s } toSet
  def symsAndEffectSyms(x: Node): (Set[Sym], Set[Sym]) = ((Set[Sym](), x.eff.hdeps.toSet) /: x.rhs) {
    case ((syms, symsEffect), s: Sym) => (syms + s, symsEffect)
    case ((syms, symsEffect), Block(_, res: Sym, _, eff)) => (syms + res, symsEffect ++ eff.hdeps)
    case ((syms, symsEffect), Block(_, _, _, eff)) => (syms, symsEffect ++ eff.hdeps)
    case (agg, _) => agg
  }

  def syms(x: Node): List[Sym] = {
    x.rhs.flatMap {
      case s: Sym => List(s)
      case b: Block => b.deps
      case _ => Nil
    } ++ x.eff.deps
  }

  def hardSyms(x: Node): Set[Sym] = {
    x.rhs.foldLeft(Set[Sym]()) {
      case (agg, s: Sym) => agg + s
      case (agg, b: Block) => agg ++ b.used
      case (agg, _) => agg
    } ++ x.eff.hdeps
  }
}

import Backend._


class GraphBuilder {
  val globalDefs = new mutable.ArrayBuffer[Node]
  val globalDefsCache = new mutable.HashMap[Sym,Node]
  val globalDefsReverseCache = new mutable.HashMap[(String,Seq[Def]),Node]

  var nSyms = 0
  def fresh = try nSyms finally nSyms += 1

  object Def {
    def unapply(xs: Def): Option[(String,List[Def])] = xs match {
      case s @ Sym(n) =>
        findDefinition(s).map(n => (n.op,n.rhs))
      case _ => None
    }
  }

  def findDefinition(s: Exp): Option[Node] = s match { case s: Sym => globalDefsCache.get(s) case _ => None }
  def findDefinition(op: String, as: Seq[Def]): Option[Node] = globalDefsReverseCache.get((op,as))

  def rewrite(s: String, as: List[Def]): Option[Exp] = None

  def reflect(s: String, as: Def*): Exp = {
    reflectEffect(s, as:_*)()()
  }

  def reflectRead(s: String, as: Def*)(efKeys: Exp) = reflectEffect(s, as:_*)(efKeys)()
  def reflectWrite(s: String, as: Def*)(efKeys: Exp) = reflectEffect(s, as:_*)()(efKeys)
  def reflectMutable(s: String, as: Def*) = reflectEffect(s, as:_*)(STORE)()

  // FIXME: issues:
  //  - write to STORE doens't really capture the meaning of free
  //  - if one of the free is DCE, the "path" through write on store is broken thus free aren't generated:
  //      x11: free(x1) // deps ...
  //      x12: free(x2) // hdeps x11
  //      x13: free(x3) // hdeps x12
  //
  //    x12 DCEed...
  //      x11: free(x1) // deps ...  <- non reachable
  //      x13: free(x3) // hdeps x12
  def reflectFree(s: String, as: Def*)(efKeys: Exp) = reflectEffect(s, as:_*)()(efKeys, STORE)
  def reflectRealloc(s: String, as: Def*)(efKeys: Exp) = reflectEffect(s, as:_*)(STORE)(efKeys, STORE)

  def reflectUnsafe(s: String, as: Def*) = reflectEffect(s, as:_*)(UNSAFE)()


  def latest(e1: Exp) = if (curLocalDefs(e1)) e1.asInstanceOf[Sym] else curBlock
  def getLastWrite(x: Exp) = curEffects.get(x) match {
    case Some((lw, _)) => lw
    case _ => latest(x)
  }

  var reflectHere = false
  def reflectEffectSummary(s: String, as: Def*)(efKeys: (Set[Exp], Set[Exp])): Exp =
    reflectEffect(s, as:_*)(efKeys._1.toSeq:_*)(efKeys._2.toSeq:_*)
  def reflectEffectSummaryHere(s: String, as: Def*)(efKeys: (Set[Exp], Set[Exp])): Exp =
    scopedReflectHere(true)(reflectEffectSummary(s, as:_*)(efKeys))
  def scopedReflectHere(flag: Boolean)(closure: => Exp): Exp = {
    val saveReflectHere = reflectHere
    reflectHere = flag
    try {
      closure
    } finally { reflectHere = saveReflectHere }
  }

  def isCurrentValue(src: Exp, value: Sym) = !curEffects.get(src).filter({ case (_, lrs) => lrs contains value }).isEmpty
  // This is the main function for reflect with explicit read/write effects
  def reflectEffect(s: String, as: Def*)(readEfKeys: Exp*)(writeEfKeys: Exp*): Exp = {
    // simple pre-construction optimization
    rewrite(s, as.toList) match {
      case Some(e) => e // found optimization (resulting in pure expressions only)
      case None => // no available optimization
        // latent effects? closures, mutable vars, ... (these are the keys!)
        val (reads, _writes) = if (s == "λ") { // NOTE: block in lambda is a latent effect for app, not declaration
          (readEfKeys.toSet, writeEfKeys.toSet)
        } else {
          val (ref, wef) = getLatentEffect(s, as:_*)
          ((ref ++ readEfKeys).toSet, (wef ++ writeEfKeys).toSet)
        }
        // FIXME(feiw) get this special case refactored.
        val writes = if (s == "reset1") _writes filter (_ != stub.Adapter.CPS) else _writes

        if (reads.nonEmpty || writes.nonEmpty) {
          lazy val res = {
            val sm = Sym(fresh)
            val (prevHard, prevSoft) = gatherEffectDeps(reads, writes, s, as:_*)
            // prevSoft --= prevHard
            val summary = EffectSummary(prevSoft, prevHard, reads, writes)
            val res = reflect(sm, s, as:_*)(summary)
            // update effect environments (curEffects, curLocalReads, and curLocalWrites)
            curLocalReads ++= reads
            curLocalWrites ++= writes
            for (key <- reads) {
              val (lw, lrs) = curEffects.getOrElse(key, (curBlock, Nil)) // FIXME really?
              curEffects += key -> (lw, res::lrs)
              if (key == STORE) curEffects += res -> (res, Nil) // Needed to notify res was defined locally
            }                                                            // Do we want it?
            for (key <- writes) { curEffects += key -> (res, Nil) }
            res
          }

          // if (writes.isEmpty) // cse for reads effects
          //   findDefinition(s,as).filter({ n =>
          //     val curValue = reads.forall(src => src != STORE && isCurrentValue(src, n.n))
          //     // if (curValue) System.out.println(s"$s $as -- DCE with ${n.n} (${reads.mkString(", ")})")
          //     curValue
          //   }).map(_.n).getOrElse(res)
          // else
            res
        } else {
          // We can run Common Subexpression Elimination (CSE) for pure nodes
          findDefinition(s,as) match {
            case Some(n) => n.n
            case None =>
              reflect(Sym(fresh), s, as:_*)()
          }
        }
    }
  }

  def gatherEffectDeps(reads: Set[Exp], writes: Set[Exp], s: String, as: Def*): (Set[Sym], Set[Sym]) = {
    val (prevHard, prevSoft) = (new mutable.ListBuffer[Sym], new mutable.ListBuffer[Sym])
    // gather effect dependencies 1): handle the write keys
    for (key <- writes) {
      curEffects.get(key) match {
        case Some((lw, lr)) =>
          val (sdeps, hdeps) = gatherEffectDepsWrite(s, as, lw, lr)
          prevSoft ++= sdeps; prevHard ++= hdeps
        case _ =>
          // write has hard dependencies on declaration (if declared locally) or block (if declared globally, i.e., out of current block)
          prevHard += latest(key);
      }
    }
    // gather effect dependencies 2): handling of reifyHere
    // reifyHere is an Effect Optimization for conditionals (if and switch)
    // it allows the block of conditionals to be aware of the `curEffects` out of the block
    // The exact demonstration of the optimization is yet unknown, should investigate.
    if (reifyHere) prevHard += curBlock
    // gather effect dependencies 3): handle read keys (i.e., reads have hard dependencies on previous write)
    for (key <- reads) {
      prevHard += getLastWrite(key)
    }
    (prevHard.toSet, prevSoft.toSet)
  }

  // Conservative handling of dependencies for write keys: (return soft deps and hard deps respectively)
  // 1) write has hard dependencies on previous write (this is conservative for array case, Store, Ctrl,...)
  // 2) write has soft dependencies on previous read (just enforcing order, do not enforcing the reads to be scheduled)
  def gatherEffectDepsWrite(s: String, as: Seq[Def], lw: Sym, lr: Seq[Sym]): (Set[Sym], Set[Sym]) =
    (if (!reflectHere) lr.toSet else Set(), Set(latest(lw))) // FIXME(feiw) why not adding soft dependencies when not reflectHere?

  // FIXME: take EffectSummary as argument?
  def reflect(x: Sym, s: String, as: Def*)(summary: EffectSummary = emptySummary): Sym = {
    val n = Node(x, s, as.toList, summary)
    globalDefs += n
    globalDefsCache(x) = n
    globalDefsReverseCache((s,n.rhs)) = n
    curLocalDefs += x
    x
  }

  /*
  NOTE: There are many problems with the approach of having
  to analyze all lambda arguments to a function to discover
  latent effects. What if the lambda is wrapped in a struct?
  In general we need to find all lambdas potentially reachable
  from an argument. This looks awfully similar to how we
  deal with aliases to mutable variables in LMS 1.x.
  (A possible view here is that tracking lambdas is the same
  as tracking aliases/separation of data structures by means
  of closure conversion).
  */

  def getLatentEffect(x: Def): (Set[Exp], Set[Exp]) = x match {
    case b: Block =>
      getEffKeys(b)
    case s: Sym =>
      findDefinition(s) match {
        case Some(Node(_, "λ", (b@Block(ins, out, ein, eout))::_, _)) =>
          getEffKeys(b)
        case _ =>
          (Set[Exp](), Set[Exp]())
      }
    case _ =>
      (Set[Exp](), Set[Exp]())
  }
  def getLatentEffect(op: String, xs: Def*): (mutable.Set[Exp], mutable.Set[Exp]) = (op, xs) match {
    case ("@", (f: Sym)+:args) => // should be a lambda. Block?
      val (reads, writes) = getLatentEffect("useless", args:_*)
      val ((freads, fwrites), argsSym) = findDefinition(f) match {
        case Some(Node(_, "λ", (b@Block(ins, out, ein, eout))::_, _)) =>
          (getEffKeys(b), ins)
        case Some(Node(_, "λforward", _, _)) => // what about doubly recursive?
          ((Set[Exp](), Set[Exp](Const("CTRL"))), Nil)
        case None => // FIXME: function argument? fac-01 test used for recursive function...
          ((Set[Exp](), Set[Exp](Const("CTRL"))), Nil)
        case Some(_) =>
          ??? // FIXME what about @, ?, array_apply => conservative write on all args?
      }

      // For @ we need to transfer effect on parameters
      // on the actual values
      for ((x: Exp, xin) <- args zip argsSym) {
          if (fwrites(xin)) writes += x
          else if (freads(xin))  reads += x
      }

      // We also need to add Const effect:
      (reads ++ freads.filter(_.isInstanceOf[Const]), writes ++ fwrites.filter(_.isInstanceOf[Const]))
    case _ =>
      val reads = new mutable.HashSet[Exp]
      val writes = new mutable.HashSet[Exp]
      for (x <- xs) {
        val (ref, wef) = getLatentEffect(x)
        reads ++= ref
        writes ++= wef
      }
      (reads, writes)
  }


  var curBlock: Sym = _ // could this become an ExplodedStruct?
  var curEffects: BlockEffect = _ // map key to write/read deps, was read?
  var curLocalDefs: Set[Exp] = _
  var curLocalReads: Set[Exp] = _
  var curLocalWrites: Set[Exp] = _
  var reifyHere: Boolean = false

  // NOTE/TODO: want to mask out purely local effects

  def getInnerNodes(b: Block): List[Node] = {
    val bound = new Bound
    bound(Graph(globalDefs.toList,b, globalDefsCache.toMap))
    val ins = b.ein::b.in
    globalDefs.toList.filter(n => ins.exists(bound.hm.getOrElse(n.n,Set())))
  }

  def getEffKeys(b: Block) = (b.eff.rkeys, b.eff.wkeys)
  def mergeEffKeys(b: Block, c: Block) =
    (b.eff.rkeys ++ c.eff.rkeys, b.eff.wkeys ++ c.eff.wkeys)

  def reify(f: => Exp): Block =  reify(0, _ => f)
  def reifyHere(f: => Exp): Block =  reify(0, xs => f, true)
  def reify(f: Exp => Exp): Block = reify(1, xs => f(xs(0)))
  def reifyHere(f: Exp => Exp): Block =  reify(1, xs => f(xs(0)), true)
  def reify(f: (Exp, Exp) => Exp): Block = reify(2, xs => f(xs(0), xs(1)))
  def reify(f: (Exp, Exp, Exp) => Exp): Block = reify(3, xs => f(xs(0), xs(1), xs(2)))
  def reify(f: (Exp, Exp, Exp, Exp) => Exp): Block = reify(4, xs => f(xs(0), xs(1), xs(2), xs(3)))

  case class BlockEffect(var map: Map[Exp,(Sym, List[Sym])], prev: BlockEffect) {
    def get(key: Exp): Option[(Sym, List[Sym])] = if (prev != null) map.get(key) orElse prev.get(key) else map.get(key)
    def getOrElse(key: Exp, default: (Sym, List[Sym])) = get(key).getOrElse(default)
    def +=(kv: (Exp, (Sym, List[Sym]))) = map += kv
  }

  def withBlockScopedEnv(here: Boolean)(closure: => Block): Block = {
    val save = curBlock
    val saveEffects = curEffects
    val saveLocalDefs = curLocalDefs
    val saveLocalReads = curLocalReads
    val saveLocalWrites = curLocalWrites
    val saveReifyHere = reifyHere
    try {
      curBlock = Sym(fresh)
      curEffects = BlockEffect(Map(), if (here) curEffects else null)
      reifyHere = here
      curLocalDefs = Set()
      curLocalReads = Set()
      curLocalWrites = Set()
      closure
    } finally {
      curBlock = save
      curEffects = saveEffects
      curLocalDefs = saveLocalDefs
      curLocalReads = saveLocalReads
      curLocalWrites = saveLocalWrites
      reifyHere = saveReifyHere
    }
  }

  def reify(arity: Int, f: List[Exp] => Exp, here: Boolean = false): Block = withBlockScopedEnv(here){
    val args = (0 until arity).toList.map(_ => Sym(fresh))
    val res = f(args)
    // remove local definitions from visible effect keys
    val reads = curLocalReads.filterNot(curLocalDefs)
    val writes = curLocalWrites.filterNot(curLocalDefs)
    // TODO: it is possible to remove the dependencies, too (--> DCE for var_set / need to investigate more)
    // for (e <- curEffects.keys if curLocalDefs(e)) curEffects -= e
    // TODO:
    //  - if tests
    //  - while tests
    //  - closure test
    var hard = writes.map(curEffects.map(_)._1)
    if (curEffects.map contains res) // if res is a local mutable (e.g. Array)
      hard += curEffects.map(res)._1
    if (hard.isEmpty)
      hard = Set(curBlock)

    Block(args, res, curBlock, EffectSummary(Set(), hard, reads, writes))
  }
}


case class Graph(val nodes: Seq[Node], val block: Block, val globalDefsCache: immutable.Map[Sym,Node]) {
  // contract: nodes is sorted topologically
}


abstract class Phase extends (Graph => Graph) {

}

// Compute liveness information and discard
// nodes not used in computing the result
class DeadCodeElim extends Phase {
  def apply(g: Graph): Graph = {

    val live = new mutable.HashSet[Sym]

    if (g.block.res.isInstanceOf[Sym])
      live += g.block.res.asInstanceOf[Sym]

    for (d <- g.nodes.reverseIterator)
      if (live(d.n)) live ++= syms(d)

    g.copy(nodes = g.nodes.filter(d => live(d.n)))
  }
}

// Compute frequency information (i.e., how
// many times a node's result is going to
// be used, in expectation)
class Flow extends Phase {

  val freq = new mutable.HashMap[Sym,Double]

  // XXX: not clear how to count effect deps
  // (e.g. 'if' shouldn't count effect as 0.5, b/c together with
  // a normal dep this is 1.0, and would thus hoist stuff)

  // XXX perhaps we need to count freqs differently (?)

  def symsFreq(x: Node): List[(Def,Double)] = x match {
    case Node(f, "λ", Block(in, y, ein, eff)::_, _) =>
      List((y,100.0)) ++ eff.deps.map(e => (e,0.001))
    case Node(_, "?", c::Block(ac,ae,ai,af)::Block(bc,be,bi,bf)::_, _) =>
      List((c,1.0),(ae,0.5),(be,0.5)) ++ af.deps.map(e => (e,0.001)) ++ bf.deps.map(e => (e,0.001))
    case _ => syms(x) map (s => (s,1.0))
  }

  def apply(g: Graph): Graph = {

    if (g.block.res.isInstanceOf[Sym])
      freq(g.block.res.asInstanceOf[Sym]) = 1.0

    for (e <- g.block.eff.deps)
      if (e.isInstanceOf[Sym])
        freq(e.asInstanceOf[Sym]) = 1.0

    for (d <- g.nodes.reverseIterator) {
      if (freq contains d.n) {
        val s = freq(d.n)
        for ((e:Sym,f) <- symsFreq(d))
          if (f > 0.5) freq(e) = (freq.getOrElse(e, 0.0) + (f*s))
      }
    }

    //freq.toList.sortBy(_._1.n).foreach(println)

    g
  }
}

// Compute bound structure information
// (i.e., which bound variables a node depends on)
class Bound extends Phase {

  val hm = new mutable.HashMap[Sym,Set[Sym]]

  def apply(g: Graph): Graph = {
    val bound = g.nodes.flatMap(boundSyms).toSet ++ g.block.bound

    // For recursive syms, we don't want to force
    // non-recursive deps into nested scopes
    // for (Node(b,"λ",_,_) <- g.nodes)
    //   hm(b) = Set()
    //
    // This explicit initialization was previously needed
    // since we used hm.getOrElse(a,Set(a)) as default below
    // (there is a choice whether undefined symbols should be
    // treated as bound or not -- this case is typically only
    // encountered for recursive lambda refs).

    for (b <- bound)
      hm(b) = Set(b)

    // Convergence loop: we want to make sure that recursive
    // refs via λforward nodes acquire the same bound information
    // as the lambda itself (which comes later in the list) and
    // hence get scheduled into the same block (see recursion_3 test).

    var more = true
    while (more) {
      more = false

      for (d <- g.nodes) {
        val b = boundSyms(d).toSet - d.n
        val newSyms = syms(d).flatMap(a => hm.getOrElse(a,Set())).toSet -- b
        more ||= (d.op == "λforward" && hm.get(d.n) != Some(newSyms))
        hm(d.n) = newSyms
      }
    }

    //hm.foreach(println)

    g
  }

}
