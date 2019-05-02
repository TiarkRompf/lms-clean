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
  abstract class Effect {
    override def toString = this match {
      case Read(x) => x.toString
      case Write(x) => x.toString + "*"
      case Alloc => "Store"
      case Other(x) => x.toString
    }
  }
  case class Read(x: Sym) extends Effect
  case class Write(x: Sym) extends Effect
  case object Alloc extends Effect
  case class Other(x: Const) extends Effect


  case class EffectSummary(sdeps: List[Sym], hdeps: List[Sym], keys: List[Effect]) {
  // case class EffectSummary(deps: List[Exp], keys: List[Exp]) {
    lazy val deps = hdeps ++ sdeps
    override def toString = if (deps.nonEmpty || keys.nonEmpty)
      keys.mkString("[", " ", ": ") + (if (sdeps.nonEmpty) sdeps.mkString("soft {", " ", "} ") else "") + (if (hdeps.nonEmpty) hdeps.mkString("hard {", " ", "}]") else "]")
    else ""
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
    def used = if (res.isInstanceOf[Sym] && !eff.deps.contains(res)) (res.asInstanceOf[Sym])::eff.deps else eff.deps
    def isPure = eff.deps == List(ein)
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
  def symsAndEffectSyms(x: Node): (Set[Sym], Set[Sym]) = ((Set[Sym](), filterSym(x.eff.deps)) /: x.rhs) {
    case ((syms, symsEffect), s: Sym) => (syms + s, symsEffect)
    case ((syms, symsEffect), Block(_, res: Sym, _, eff)) => (syms + res, filterSym(eff.deps) ++ symsEffect)
    case ((syms, symsEffect), Block(_, _, _, eff)) => (syms, filterSym(eff.deps) ++ symsEffect)
    case (agg, _) => agg
  }

  def syms(x: Node): List[Sym] = {
    x.rhs.flatMap {
      case s: Sym => List(s)
      case b: Block => b.used
      case _ => Nil
    } ++ x.eff.deps.collect { case s: Sym => s }
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
    reflectEffect(s, as:_*)()
  }

  def reflectRead(s: String, as: Def*)(efKeys: Exp) = efKeys match {
    case key: Sym => reflectEffect(s, as:_*)(Read(key))
    case _ => ???
  }
  def reflectWrite(s: String, as: Def*)(efKeys: Exp) = efKeys match {
    case key: Sym => reflectEffect(s, as:_*)(Write(key))
    case _ => ???
  }
  def reflectEffect(s: String, as: Def*)(efKeys: Effect*): Exp = {
    // rewrite?
    rewrite(s,as.toList) match {
      case Some(e) => e
      case None =>

        // latent effects? closures, mutable vars, ... (these are the keys!)
        val efKeys2_ = (if (s == "λ") efKeys else // NOTE: block in lambda is a latent effect for app, not declaration
          (as.toList.flatMap(getLatentEffect) ++ efKeys)).distinct
        val efKeys2 = s match {
          case "reset1" => efKeys2_ filter (_ != stub.Adapter.CPS)
          case _ => efKeys2_
        }

        //



        // effects or pure? // FIXME: only reads?
        if (efKeys2.nonEmpty) {
          val sm = Sym(fresh)
          // gather effect dependencies
          // def latest(e1: Exp, e2: Exp) = if (curLocalDefs(e1)) e1 else e2
          // val prev = efKeys2.map(e => curEffects.getOrElse(e,latest(e,curBlock))).distinct

          val prevSoft = new mutable.HashSet[Sym]
          val prevHard = new mutable.HashSet[Sym]
          for (e <- efKeys2) e match {
            case Read(v) => prevHard += getLastWrite(v) // still need curent block for loops!
            case Alloc   => prevHard += curBlock
            case Write(v)=>
              val (lw, lrs) = curEffects.getOrElse(v, (curBlock, Nil))
              prevHard += lw // strong dep last write
              prevSoft ++= lrs // soft dep reads since last write
            case Other(s)=> // CTRL STORE CPS
              prevHard += getLastWrite(s)
          }
          // prevSoft --= prevHard
          val res = reflect(sm, s, as:_*)(prevSoft.toSeq:_*)(prevHard.toSeq:_*)(efKeys2:_*)
          for (e <- efKeys2) e match {
            case Alloc    => curEffects = curEffects + (res -> (res, Nil))
            case Read(v)  =>
              val (lw, lrs) = curEffects.getOrElse(v, (curBlock, Nil)) // FIXME really?
              curEffects = curEffects + (v -> (lw, res::lrs))
            case Write(v) =>
              val (lw, _) = curEffects.getOrElse(v, (curBlock, Nil)) // FIXME really?
              curEffects = curEffects + (v -> (res, Nil))
            case Other(v) =>
              curEffects = curEffects + (v -> (res, Nil))
          }
          res
        } else {
          // cse? never for effectful stm
          findDefinition(s,as) match {
            case Some(n) => n.n
            case None =>
              reflect(Sym(fresh), s, as:_*)()()()
          }
        }
    }
  }

  def reflect(x: Sym, s: String, as: Def*)(sEfDeps: Sym*)(hEfDeps: Sym*)(efKeys: Effect*): Sym = {
    val n = Node(x,s,as.toList,EffectSummary(sEfDeps.toList, hEfDeps.toList, efKeys.toList))
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
          Nil
      }
    case _ =>
      Nil
  }


  var curBlock: Sym = _ // could this become an ExplodedStruct?
  var curEffects: Map[Exp,(Sym, List[Sym])] = _ // map key to write/read deps
  var curLocalDefs: Set[Exp] = _

  def getLastWrite(x: Exp) = curEffects.get(x) match {
    case Some((lw, _)) => lw
    case _ => curBlock
  }

  // NOTE/TODO: want to mask out purely local effects
  def isPure(b: Block) = b.eff.deps == List(b.ein)

  def getInnerNodes(b: Block): List[Node] = {
    val bound = new Bound
    bound(Graph(globalDefs.toList,b, globalDefsCache.toMap))
    val ins = b.ein::b.in
    globalDefs.toList.filter(n => ins.exists(bound.hm.getOrElse(n.n,Set())))
  }

  def getEffKeys(b: Block): List[Effect] = b.eff.keys

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
      val hard = new mutable.HashSet[Sym]
      val keys = new mutable.ListBuffer[Effect]
      for ((key, (lw, lrs)) <- curEffects) {
        if (!curLocalDefs(key)) key match {
          case k: Const => keys += Other(k)
          case s: Sym   => keys += (if (lw == curBlock) Read(s) else Write(s))
        }

        hard += lw
        hard ++= lrs
      }
      Block(args, res, block, EffectSummary(Nil, if (hard.size == 0) List(block) else hard.toList, keys.toList))
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






