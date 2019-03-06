package lms.core


import scala.collection.mutable

object Backend {

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

  case class EffectSummary(deps: List[Exp], keys: List[Exp]) {
    override def toString = if (deps.nonEmpty || keys.nonEmpty)
      s"[${keys.mkString(" ")}: ${deps.mkString(" ")}]" else ""
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
    - Block should separate out input effects, too
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
    def used = (res::eff.deps).distinct collect { case s: Sym => s }
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

  def syms(x: Node): List[Sym] = 
    x.rhs.flatMap {
      case s: Sym => List(s) 
      case b: Block => b.used
      case _ => Nil
    } ++ x.eff.deps.collect { case s: Sym => s }
}

import Backend._


class GraphBuilder {
  val globalDefs = new mutable.ArrayBuffer[Node]

  var nSyms = 0
  def fresh = try nSyms finally nSyms += 1

  object Def {
    def unapply(xs: Def): Option[(String,List[Def])] = xs match {
      case s @ Sym(n) =>
        globalDefs.find(_.n == s).map(n => (n.op,n.rhs))
      case _ => None
    }
  }

  var name: String = ""

  var rewrites: (String => Any => Option[Exp]) = (s => x => None)

  def rewrite(f: PartialFunction[Any,Exp]) = {
    val old = rewrites
    val f1 = ((x:Any) => if (f.isDefinedAt(x)) Some(f(x)) else None)
    rewrites = (s => x => f1(x).orElse(old(s)(x)))
  }


  def reflect(s: String, as: Def*): Exp = {
    reflectEffect(s, as:_*)()
  }

  def reflectEffect(s: String, as: Def*)(efs: Exp*): Exp = {
    // rewrite?
    rewrites(name)((s,as.toList)) match {
      case Some(e) => e 
      case None => 

      // latent effects? closures, mutable vars, ... (these are the keys!)
      val efs2 = (as.toList.flatMap(getLatentEffect) ++ efs).distinct

      // effects or pure?
      if (efs2.nonEmpty) {
        val sm = Sym(fresh) 
        // gather effect dependencies
        val prev = effectForKeys(curBlock,efs2)
        try reflect(sm, s, as:_*)(prev:_*)(efs2:_*)
        finally curBlock = updateEffectForKeys(curBlock,efs2,sm)
      } else {
        // cse? never for effectful stm
        globalDefs.find(n => n.op == s && n.rhs == as) match {
          case Some(n) => n.n
          case None =>
            reflect(Sym(fresh), s, as:_*)()()
        }
      }
    }
  }
  
  def reflect(x: Sym, s: String, as: Def*)(efDeps: Exp*)(efKeys: Exp*): Exp = {
    globalDefs += Node(x,s,as.toList,EffectSummary(efDeps.toList, efKeys.toList))
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

  // TODO: x might be a block!
  def getLatentEffect(x: Def) = globalDefs.find(_.n == x) match {
    case Some(Node(_, "λ", List(b @ Block(ins, out, ein, eout)), _)) =>
      getEffKeys(b)
    case _ => 
      Nil
  }


  var curBlock: Effect = _ // could this become an ExplodedStruct?

  type Effect = List[Exp]

  // es the accumulated effect (list of effectful stms) of the current block
  // efs is the set of effect keys of interest
  // return a new effect (subset of es) based on the keys efs
  def effectForKeys(es: Effect, efs: List[Exp]): Effect = {
    es.flatMap { e =>
      globalDefs.find(_.n == e) match {
        case Some(Node(n,op,rhs,eff)) =>
          if ((efs contains n) || (efs intersect eff.keys).nonEmpty) {
            List(e)
          } else {
            effectForKeys(eff.deps, efs)
          }
        case _ => List(e)
      }
    }.distinct
  }

  // remove all dependencies from es (typically curBlock) that are implied 
  // by current node (sm, with effectKeys efs)
  def updateEffectForKeys(es: Effect, efs: List[Exp], sm: Sym): Effect = {
    val prev = effectForKeys(es,efs)
    sm::(es.filterNot(prev.toSet))
    // TODO: is this enough? (when following individual keys?)
  }

  
  // NOTE/TODO: want to mask out purely local effects
  def isPure(b: Block) = b.eff.deps == List(b.ein)

  def getEffKeys0(b: Block): List[Exp] = {
    // TODO -- cleanup: performance!!!

    val bound = new Bound
    bound(Graph(globalDefs.toList,b))

    val nodes = globalDefs.toList.filter(n => bound.hm.getOrElse(n.n,Set())(b.ein))

    // TODO: remove deps on nodes that are bound inside??
    nodes.flatMap(_.eff.keys).distinct
  }

  def getEffKeys(b: Block): List[Exp] = b.eff.keys



  def reify(x: => Exp): Block = {
    val save = curBlock
    try {
      val block = Sym(fresh)
      curBlock = List(block)
      val res = x 
      val b0 = Block(Nil, res, block, EffectSummary(curBlock, Nil)) // XXX FIXME temp 
      Block(Nil, res, block, EffectSummary(curBlock, getEffKeys0(b0)))
    } finally {
      curBlock = save
    }
  }
  def reify(x: Exp => Exp): Block = {
    val save = curBlock
    try {
      val block = Sym(fresh)
      val arg = Sym(fresh)
      curBlock = List(block)
      val res = x(arg) 
      val b0 = Block(arg::Nil, res, block, EffectSummary(curBlock, Nil)) // XXX FIXME temp 
      Block(arg::Nil, res, block, EffectSummary(curBlock, getEffKeys0(b0)))
    } finally {
      curBlock = save
    }
  }

}


case class Graph(val nodes: Seq[Node], val block: Block) {
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

    Graph(g.nodes.filter(d => live(d.n)), g.block)
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

    // for recursive syms, we don't want to force
    // non-recursive deps into nested scopes
    for (Node(b,"λ",_,_) <- g.nodes)
      hm(b) = Set()

    for (b <- bound) 
      hm(b) = Set(b)

    for (d <- g.nodes) {
      val b = boundSyms(d).toSet - d.n
      hm(d.n) = syms(d).flatMap(a => hm.getOrElse(a,Set(a))).toSet -- b
    }

    //hm.foreach(println)

    g
  }

}






