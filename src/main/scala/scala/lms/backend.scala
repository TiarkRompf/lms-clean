package scala.lms

/*
  LMS compiler back-end in one file.
*/

import scala.collection.mutable

object Backend {

  abstract class Def
  abstract class Exp extends Def

  case class Sym(n: Int) extends Exp {
    override def toString = s"x$n"
  }
  case class Const(x: Any) extends Exp

  case class Node(n: Sym, op: String, rhs: List[Def]) {
    override def toString = s"$n = ($op ${rhs.mkString(" ")})"
  }

  case class Block(in: List[Sym], res: Exp, eff: Exp) extends Def

  // where should this go?
  def boundSyms(x: Node): Seq[Sym] = blocks(x) flatMap (_.in)

  def blocks(x: Node): List[Block] = 
    x.rhs.collect { case a @ Block(_,_,_) => a }

  def directSyms(x: Node): List[Sym] = 
    x.rhs.flatMap { 
      case s: Sym => List(s) 
      case _ => Nil
    }

  def syms(x: Node): List[Sym] = 
    x.rhs.flatMap { 
      case s: Sym => List(s) 
      case Block(_, s: Sym, e: Sym) => List(s,e)
      case Block(_, s: Sym, _) => List(s)
      case Block(_, _, e: Sym) => List(e)
      case _ => Nil
    } diff boundSyms(x)
}

import Backend._


class GraphBuilder {
  val globalDefs = new mutable.ArrayBuffer[Node]

  var nSyms = 0
  def fresh = try nSyms finally nSyms += 1

  def reflect(s: String, as: Def*): Exp = {
    reflect(Sym(fresh), s, as:_*)
  }
  def reflectEffect(s: String, as: Def*): Exp = {
    val sm = Sym(fresh) 
    try reflect(sm, s, (as :+ curBlock):_*) finally curBlock = sm
  }
  def reflect(x: Sym, s: String, as: Def*): Exp = {
    globalDefs += Node(x,s,as.toList)
    x
  }

  var curBlock: Sym = _

  def reify(x: => Exp): Block = {
    val save = curBlock
    try {
      val block = Sym(fresh)
      curBlock = block
      val res = x 
      Block(block::Nil, res, curBlock)
    } finally {
      curBlock = save
    }
  }
  def reify(x: Exp => Exp): Block = {
    val save = curBlock
    try {
      val block = Sym(fresh)
      val arg = Sym(fresh)
      curBlock = block
      val res = x(arg) 
      Block(arg::block::Nil, res, curBlock)
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
// nodes that used in computing the result
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
    case Node(f, "λ", List(Block(in, y, eff))) => 
      List((y,100),(eff,0.001))
    case Node(_, "?", List(c, Block(ac,ae,af), Block(bc,be,bf))) => 
      List((c,1.0),(ae,0.5),(af,0.001),(be,0.5),(bf,0.001))
    case _ => syms(x) map (s => (s,1.0))
  }

  def apply(g: Graph): Graph = {

    if (g.block.res.isInstanceOf[Sym])
      freq(g.block.res.asInstanceOf[Sym]) = 1.0

    if (g.block.eff.isInstanceOf[Sym])
      freq(g.block.eff.asInstanceOf[Sym]) = 1.0

    for (d <- g.nodes.reverseIterator) {
      if (freq contains d.n) {
        val s = freq(d.n)
        for ((e:Sym,f) <- symsFreq(d))
          freq(e) = (freq.getOrElse(e, 0.0) + (f*s))
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
    val bound = g.nodes.flatMap(boundSyms).toSet ++ g.block.in

    // for recursive syms, we don't want to force
    // non-recursive deps into nested scopes
    for (Node(b,"λ",_) <- g.nodes)
      hm(b) = Set()

    for (b <- bound) 
      hm(b) = Set(b)

    for (d <- g.nodes) {
      val b = boundSyms(d).toSet - d.n
      hm(d.n) = syms(d).flatMap(hm).toSet -- b
    }

    //hm.foreach(println)

    g
  }

}


abstract class Traverser {

  val bound = new Bound

  var path = List[Sym]()
  var inner: Seq[Node] = _

  def withScope[T](p: List[Sym], ns: Seq[Node])(b: =>T): T = {
    val (path0, inner0) = (path, inner)
    path = p; inner = ns;
    try b finally { path = path0; inner = inner0 }
  }

  def traverse(y: Block, extra: Sym*): Unit = {
    val path1 = y.in ++ extra.toList ++ path

    // Note: frequency must be recalculated for each subgraph
    // Question: is that costly? Can/should we avoid it?
    val flow = new Flow
    flow(new Graph(inner, y))

    def scheduleHere(d: Node) = {
      // Should node d be scheduled here? It must be:
      // (1) available: not dependent on other bound vars
      // (2) used at least as often as the block result
      bound.hm(d.n) -- path1 - d.n == Set() && 
      flow.freq.getOrElse(d.n,0.0) >= 1
    }
    // NOTE: nodes with freq = 0 could be removed altogether!

    val (outer1, inner1) = inner.partition(scheduleHere)

    withScope(path1, inner1) {
      traverse(outer1, y)
    }
  }
  def traverse(ns: Seq[Node], res: Block): Unit = {
    ns.foreach(traverse)
  }
  def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      // special case λ: add free var f
      traverse(y, f)
    case n @ Node(f, op, es) =>
      // generic traversal: go into all blocks
      for (e @ Block(_,_,_) <- es)
        traverse(e)
  }

  def apply(g: Graph): Unit = {

    bound(g)

    withScope(Nil, g.nodes) {
      traverse(g.block)
    }
  }

}

object utils {
    // XXX do without
  def captureOut(func: => Any): String = {
    val source = new java.io.ByteArrayOutputStream()
    withOutput(new java.io.PrintStream(source))(func)
    source.toString    
  }
  def withOutput[T](out: java.io.PrintStream)(f: => Unit): Unit = {
    scala.Console.withOut(out)(scala.Console.withErr(out)(f))
  }
}

class CodeGen extends Traverser {

  def emit(s: String) = println(s)

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    emit(s"// in: ${y.in}")
    super.traverse(ns, y)
    emit(y.res.toString + " // out effect: " + y.eff.toString)
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      emit(s"$f = (λ {")
      traverse(y, f)
      emit(s"})")
    case n @ Node(f, op, es) =>
      val ss = es map { 
        case e @ Block(_,_,_) => "{" + utils.captureOut(traverse(e)) + "}" // FIXME freq!!
        case e => e.toString
      }
      emit(s"$f = ($op ${ss.mkString(" ")})")
  }
}

class ScalaCodeGen extends Traverser {

  def emit(s: String) = println(s)

  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x) => x.toString
  }

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    super.traverse(ns, y)
    emit(quote(y.res))
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int = {")
      // see what becomes available given new bound vars
      traverse(y, f)
      emit(s"}")
    case n @ Node(f,"?",List(c,a:Block,b:Block)) => 
      emit(s"val $f = if (${quote(c)} != 0) {")
      traverse(a)
      emit(s"} else {")
      traverse(b)
      emit(s"}")
    case n @ Node(s,"+",List(x,y)) => 
      emit(s"val $s = ${quote(x)} + ${quote(y)}")
    case n @ Node(s,"-",List(x,y)) => 
      emit(s"val $s = ${quote(x)} - ${quote(y)}")
    case n @ Node(s,"*",List(x,y)) => 
      emit(s"val $s = ${quote(x)} * ${quote(y)}")
    case n @ Node(s,"@",List(x,y,ctl)) => 
      emit(s"val $s = ${quote(x)}(${quote(y)})")
    case n @ Node(s,"P",List(x,ctl)) => 
      emit(s"val $s = println(${quote(x)})")
    case n @ Node(_,_,_) => 
      emit(s"??? " + n.toString)
  }
}


class CompactScalaCodeGen extends Traverser {

  val rename = new mutable.HashMap[Sym,String]

  var doRename = false

  def emit(s: String) = println(s)

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    // how many times a sym is used locally
    val hm = new mutable.HashMap[Sym,Int]
    
    // how many times a sym is used in an inner scope
    val hmi = new mutable.HashMap[Sym,Int]

    // lookup sym -> node for locally defined nodes
    val df = new mutable.HashMap[Sym,Node]

    // count how many times a node is used at the current level
    if (y.res.isInstanceOf[Sym]) hm(y.res.asInstanceOf[Sym]) = 1
    for (n <- ns) {
      df(n.n) = n
      for (s <- directSyms(n)) // exclude refs through blocks
        hm(s) = hm.getOrElse(s,0) + 1
    }

    // check if a node is used from some inner scope
    for (n <- inner) {
      for (s <- syms(n))
        hmi(s) = hmi.getOrElse(s,0) + 1
    }

    val save = shouldInline

    // should a definition be inlined or let-inserted?
    // XXX do we need to do something more special for effects?
    shouldInline = { (n: Sym) => 
      if ((df contains n) &&              // locally defined
          (hm.getOrElse(n, 0) == 1) &&    // locally used exactly once
          (hmi.getOrElse(n, 0) == 0))      // not used in nested scopes
          Some(df(n))
      else None }

    // only emit statements if not inlined
    for (n <- ns) {
      if (shouldInline(n.n).isEmpty)
        traverse(n)
    }

    print(shallow(y.res))

    shouldInline = save
  }

  var shouldInline: Sym => Option[Node] = (_ => None)

  object InlineSym {
    def unapply(x: Sym) = shouldInline(x)
  }

  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(x) => x.toString
  }

  def shallow(n: Def): String = n match {
    case InlineSym(n) => shallow(n)
    case _ => quote(n)
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  def shallow(n: Node): String = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      val x = y.in.head
      // XXX what should we do for functions? 
      // proper inlining will likely work better 
      // as a separate phase b/c it may trigger
      // further optimizations
      s"({ ${quote(x)}: Int => " +
      utils.captureOut(traverse(y,f)) +
      s"})"
    case n @ Node(f,"?",List(c,a:Block,b:Block)) => 
      s"if (${quote(c)} != 0) {" +
      utils.captureOut(traverse(a)) +
      s"} else {" +
      utils.captureOut(traverse(b)) +
      s"}"
    case n @ Node(s,"+",List(x,y)) => 
      s"${shallow(x)} + ${shallow(y)}"
    case n @ Node(s,"-",List(x,y)) => 
      s"${shallow(x)} - ${shallow(y)}"
    case n @ Node(s,"*",List(x,y)) => 
      s"${shallow(x)} * ${shallow(y)}"
    case n @ Node(s,"@",List(x,y,ctl)) => 
      s"${shallow(x)}(${shallow(y)})"
    case n @ Node(s,"P",List(x,ctl)) => 
      s"println(${shallow(x)})"
    case n @ Node(_,_,_) => 
      s"??? " + n.toString
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int = {")
      traverse(y, f)
      emit(s"}")
    case n @ Node(s,"P",_) => // Unit result
      emit(shallow(n))
    case n @ Node(s,_,_) => 
      emit(s"val ${quote(s)} = " + shallow(n))
  }

  override def apply(g: Graph) = {
    super.apply(g)
    println
  }
}





class FrontEnd {

  var g: GraphBuilder = null

  case class INT(x: Exp) {
    def +(y: INT): INT = INT(g.reflect("+",x,y.x))
    def -(y: INT): INT = INT(g.reflect("-",x,y.x))
    def *(y: INT): INT = INT(g.reflect("*",x,y.x))
  }

  def IF(c: INT)(a: => INT)(b: => INT): INT = {
    // TODO: effect polymorphism!
    INT(g.reflect("?",c.x,g.reify(a.x),g.reify(b.x)))
  }
  
  def APP(f: Exp, x: INT): INT = 
    INT(g.reflectEffect("@",f,x.x))

  def PRINT(x: INT): Unit =
    g.reflectEffect("P",x.x)

  def FUN(f: INT => INT): INT => INT = FUN((_,x) => f(x))

  def FUN(f: ((INT=>INT),INT) => INT): INT => INT = {
    val fn = Sym(g.fresh)
    //val xn = Sym(g.fresh)
    val f1 = (x: INT) => APP(fn,x)
    g.reflect(fn,"λ",g.reify(xn => f(f1,INT(xn)).x))
    f1
  }

  implicit def liftInt(x: Int): INT = INT(Const(x))

  def program(body: INT => INT): Graph = {
    assert(g == null)
    g = new GraphBuilder
    try {
      val block = g.reify { arg => body(INT(arg)).x }
      Graph(g.globalDefs, block)
    } finally g = null
  }


}


// Infrastructure TODOs:
// - proper test cases
//   - code motion
//   - mutual recursion: ackermann 
// - types
// - compact printer (--> regalloc?)


// DONE: cost-based code motion

// DONE: local liveness and compact printer

// DONE: canonicalize names in generated code

// TODO: mutual recursion

// TODO: fine-grained effects

// TODO: lms tutorials & more ...

// TODO: low-level:
// - CPS conversion
// - closure conversion
// - register allocation
// - x86 assembly

