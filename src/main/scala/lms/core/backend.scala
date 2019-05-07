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
  case class Const(x: Any) extends Exp

  // A list of effect keys (mutable vars, ...) and
  // dependencies (previous writes, ...)
  //
  // TOOD: this can be made more fine-grained, i.e.,
  // distinguish may/must and read/write effects,
  // soft dependencies, ...
  case class EffectSummary(sdeps: Set[Sym], hdeps: Set[Sym], rkeys: Set[Exp], wkeys: Set[Exp], mayHdeps: Map[Sym,Sym]) {
  // case class EffectSummary(deps: List[Exp], keys: List[Exp]) {
    lazy val keys = rkeys ++ wkeys
    lazy val deps = sdeps ++ hdeps
    def isEmpty = sdeps.isEmpty && hdeps.isEmpty && rkeys.isEmpty && wkeys.isEmpty
    def repr(f: Exp => String) = {
      val keyString = (if (rkeys.nonEmpty) rkeys.map(f).mkString(" ") + " " else "") + (if (wkeys.nonEmpty) wkeys.map(f(_) + "*").mkString(" ") else "")
      val depString = (if (sdeps.nonEmpty) sdeps.map(f).mkString(" ") else "_") + ", " +
      (if (hdeps.nonEmpty) hdeps.map(f).mkString(" ") else "_") + ", " +
      (if (mayHdeps.nonEmpty) mayHdeps.map(x => f(x._1) + " -> " + f(x._2)).mkString(" ") else "_")
      "[" + keyString + ": " + depString + "]"
    }
    override def toString = if (!isEmpty) {
      repr(_.toString)
    } else ""
  }

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
      (guess: typeclass to/fro conversion for FrontEnd)
  */


  // A definition is part of the right-hand side of a
  // node definition.
  abstract class Def

  // inputs, result, effect input, effect output summary
  case class Block(in: List[Sym], res: Exp, ein: Sym, eff: EffectSummary) extends Def {
    def bound = ein::in
    def used = res match {
      case res: Sym => eff.hdeps + res
      case _ => eff.deps
    }
    def isPure = eff.deps == Set(ein)
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
      case b: Block => b.used
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

  def reflectRead(s: String, as: Def*)(efKeys: Exp*) = reflectEffect(s, as:_*)(efKeys:_*)()
  def reflectWrite(s: String, as: Def*)(efKeys: Exp*) = reflectEffect(s, as:_*)()(efKeys:_*)
  def reflectMutable(s: String, as: Def*) = reflectEffect(s, as:_*)(Const("STORE"))()


  def latest(e1: Exp) = if (curLocalDefs(e1)) e1.asInstanceOf[Sym] else curBlock
  def getLastWrite(x: Exp) = curEffects.get(x) match {
    case Some((lw, _)) => lw
    case _ => latest(x)
  }

  def reflectEffectSummary(s: String, as: Def*)(efKeys: (Set[Exp], Set[Exp])): Exp =
    reflectEffect(s, as:_*)(efKeys._1.toSeq:_*)(efKeys._2.toSeq:_*)
  def reflectEffect(s: String, as: Def*)(readEfKeys: Exp*)(writeEfKeys: Exp*): Exp = {
    // rewrite?
    rewrite(s,as.toList) match {
      case Some(e) => e
      case None =>

        // latent effects? closures, mutable vars, ... (these are the keys!)
        val (reads, _writes) = if (s == "λ") { // NOTE: block in lambda is a latent effect for app, not declaration
          (readEfKeys, writeEfKeys)
        } else {
          val reads = new mutable.HashSet[Exp]
          reads ++= readEfKeys
          val writes = new mutable.HashSet[Exp]
          writes ++= writeEfKeys
          for (a <- as) {
            val (ref, wef) = getLatentEffect(a)
            reads ++= ref
            writes ++= wef
          }
          (reads.toSeq, writes.toSeq)
        }
        val writes = if (s == "reset1") _writes filter (_ != stub.Adapter.CPS) else _writes

        // effects or pure? // FIXME: only reads?
        if (reads.nonEmpty || writes.nonEmpty) {
          val sm = Sym(fresh)
          // gather effect dependencies
          val prevHard = new mutable.ListBuffer[Sym]
          val prevSoft = writes.flatMap(e => curEffects.get(e) match {
            case Some((lw, lr)) => prevHard += latest(lw); lr
            case _ => prevHard += latest(e); Nil // not sure the condition is required. locally defined => effect on Store added an effect to the map?
          })
          prevHard ++= reads.map {
            case Const("STORE") => curBlock
            case e => getLastWrite(e) // depends if other writes later??
          }

          // prevSoft --= prevHard
          val res = reflect(sm, s, as:_*)(prevSoft.toSeq:_*)(prevHard.toSeq:_*)(reads:_*)(writes:_*)
          for (key <- reads) key match {
            case Const("STORE") => curEffects = curEffects + (res -> (res, Nil))
            case _ =>
              val (lw, lrs) = curEffects.getOrElse(key, (curBlock, Nil)) // FIXME really? mayHdeps?
              curEffects = curEffects + (key -> (lw, res::lrs))
          }
          for (key <- writes) curEffects = curEffects + (key -> (res, Nil))
          res
        } else {
          // cse? never for effectful stm
          findDefinition(s,as) match {
            case Some(n) => n.n
            case None =>
              reflect(Sym(fresh), s, as:_*)()()()()
          }
        }
    }
  }

  // FIXME: take EffectSummary as argument?
  def reflect(x: Sym, s: String, as: Def*)(sEfDeps: Sym*)(hEfDeps: Sym*)(readEfKeys: Exp*)(writeEfKeys: Exp*): Sym = {
    val n = Node(x,s,as.toList,EffectSummary(sEfDeps.toSet, hEfDeps.toSet, readEfKeys.toSet, writeEfKeys.toSet, Map()))
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

  def getLatentEffect(x: Def) = x match {
    case b: Block =>
      getEffKeys(b)
    case s: Sym =>
      findDefinition(s) match {
        case Some(Node(_, "λ", List(b @ Block(ins, out, ein, eout)), _)) =>
          getEffKeys(b)
        case _ =>
          (Nil, Nil)
      }
    case _ =>
      (Nil, Nil)
  }


  var curBlock: Sym = _ // could this become an ExplodedStruct?
  var curEffects: Map[Exp,(Sym, List[Sym])] = _ // map key to write/read deps
  var curLocalDefs: Set[Exp] = _

  // NOTE/TODO: want to mask out purely local effects
  def isPure(b: Block) = b.eff.deps == List(b.ein)

  def getInnerNodes(b: Block): List[Node] = {
    val bound = new Bound
    bound(Graph(globalDefs.toList,b, globalDefsCache.toMap))
    val ins = b.ein::b.in
    globalDefs.toList.filter(n => ins.exists(bound.hm.getOrElse(n.n,Set())))
  }

  def getEffKeys(b: Block) = (b.eff.rkeys, b.eff.wkeys)
  def mergeEffKeys(b: Block, c: Block) =
    (b.eff.rkeys ++ c.eff.rkeys, b.eff.wkeys ++ c.eff.wkeys)

  def reify(x: => Exp): Block =  reify(0, xs => x )
  def reify(x: Exp => Exp): Block = reify(1, xs => x(xs(0)))
  def reify(x: (Exp, Exp) => Exp): Block = reify(2, xs => x(xs(0), xs(1)))
  def reify(x: (Exp, Exp, Exp) => Exp): Block = reify(3, xs => x(xs(0), xs(1), xs(2)))
  def reify(x: (Exp, Exp, Exp, Exp) => Exp): Block = reify(4, xs => x(xs(0), xs(1), xs(2), xs(3)))

  def reify(arity: Int, x: List[Exp] => Exp): Block = {
    val save = curBlock
    val saveEffects = curEffects
    val saveLocalDefs = curLocalDefs
    try {
      val block = Sym(fresh)
      val args = (0 until arity).toList.map(_ => Sym(fresh))
      curBlock = block
      curEffects = Map()
      curLocalDefs = Set()
      val res = x(args)
      // remove local definitions from visible effect keys
      // TODO: it is possible to remove the dependencies, too (--> DCE for var_set / need to investigate more)
      // for (e <- curEffects.keys if curLocalDefs(e)) curEffects -= e
      // TODO:
      //  - if tests
      //  - while tests
      //  - closure test
      val soft = new mutable.HashSet[Sym]
      val hard = new mutable.HashSet[Sym]
      val rkeys = new mutable.ListBuffer[Exp]
      val wkeys = new mutable.ListBuffer[Exp]
      var mayHdeps = Map[Sym,Sym]()
      for ((key, (lw, lrs)) <- curEffects) {
        if (key == res) // if res is mutable (e.g. Array)
            hard += lw
        else if (!curLocalDefs(key)) key match {
          case Const("STORE") =>
            rkeys += key
          case Const(_) =>
            wkeys += key
            hard += lw
          case s: Sym   =>
            if (lw == curBlock) { // no write in this block
              rkeys += s
              soft += lw
              soft ++= lrs // necessary?
            } else {
              wkeys += s
              mayHdeps = mayHdeps + (s -> lw) // what about reads?
              // hard += lw
              // hard ++= lrs
            }
        }
      }
      // soft --= hard
      Block(args, res, block, EffectSummary(soft.toSet, if (hard.size == 0) Set(block) else hard.toSet, rkeys.toSet, wkeys.toSet, mayHdeps))
    } finally {
      curBlock = save
      curEffects = saveEffects
      curLocalDefs = saveLocalDefs
    }
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

/*
 * Trasform all may dependencies in hard dependencies without analysis
 */
object HardenMayHardDeps extends Phase {
  def forceMayHDeps(block: Def) = block match {
    case block: Block => block.copy(eff = block.eff.copy(hdeps = block.eff.hdeps ++ block.eff.mayHdeps.values))
    case o => o
  }

  def apply(g: Graph): Graph = {
    g.copy(nodes = g.nodes.map({ d =>
      d.copy(rhs = d.rhs.map(forceMayHDeps))
    }), block = forceMayHDeps(g.block).asInstanceOf[Block])
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
    case Node(f, "λ", List(Block(in, y, ein, eff)), _) =>
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






