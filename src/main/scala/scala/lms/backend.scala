package scala.lms

/*
  LMS in one file.
  A full compiler back-end.

  What is the goal?

  Questions, new and old:
  - compound statements (multiple results)?
  - effect polymorphism for if/else (and lambda?)

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

  case class Block(in: List[Sym], res: Exp) extends Def

    // where should this go?
    def boundSyms(x: Node): Seq[Sym] = blocks(x) flatMap (_.in)

  def blocks(x: Node): List[Block] = 
    x.rhs.collect { case a @ Block(_,_) => a }

  def syms(x: Node): List[Sym] = 
    x.rhs.collect { 
      case s: Sym => s 
      case Block(_, s: Sym) => s
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
    reflect(Sym(fresh), s, (as :+ curBlock):_*)
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
      Block(block::Nil, res)
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
      Block(arg::block::Nil, res)
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

  def symsFreq(x: Node): List[(Def,Double)] = x match {
    case Node(f, "λ", List(Block(in,y: Sym))) => 
      List((y,100))
    case Node(_, "?", List(c, Block(ac,ae), Block(bc,be))) => 
      List((c,1.0),(ae,0.5),(be,0.5))
    case _ => syms(x) map (s => (s,1.0))
  }

  def apply(g: Graph): Graph = {

    if (g.block.res.isInstanceOf[Sym])
      freq(g.block.res.asInstanceOf[Sym]) = 1.0

    for (d <- g.nodes.reverseIterator) {
      if (freq contains d.n) {
        val s = freq(d.n)
        for ((e:Sym,f) <- symsFreq(d))
          freq(e) = (freq.getOrElse(e, 0.0) + (f*s))
      }
    }

    //live.toList.sortBy(_._1.n).foreach(println)

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
      outer1.foreach(traverse)
    }
  }
  def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      // special case λ: add free var f
      traverse(y, f)
    case n @ Node(f, op, es) =>
      // generic traversal: go into all blocks
      for (e @ Block(_,_) <- es)
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

  override def traverse(y: Block, extra: Sym*): Unit = {
    emit(s"// in: ${y.in}")
    super.traverse(y, extra:_*)
    emit(y.res.toString)
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      emit(s"$f = (λ {")
      traverse(y, f)
      emit(s"})")
    case n @ Node(f, op, es) =>
      val ss = es map { 
        case e @ Block(_,_) => "{" + utils.captureOut(traverse(e)) + "}" // FIXME freq!!
        case e => e.toString
      }
      emit(s"$f = ($op ${ss.mkString(" ")})")
  }
}

class ScalaCodeGen extends Traverser {

  def emit(s: String) = println(s)

  override def traverse(y: Block, extra: Sym*): Unit = {
    //emit(s"// ctl: ${y.ctl}")
    super.traverse(y, extra:_*)
    emit(quote(y.res))
  }

  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x) => x.toString
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block)) => 
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int = {")
      // see what becomes available given new bound vars
      traverse(y, f)
      emit(s"}")
    case n @ Node(f,"?",List(c,a:Block,b:Block)) => 
      emit(s"val $f = if ($c != 0) {")
      traverse(a) // FIXME: other sym than f for bound var?
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
    case n @ Node(_,_,_) => 
      emit(s"??? " + n.toString)
  }
}





class FrontEnd {

  val g = new GraphBuilder

  case class INT(x: Exp) {
    def +(y: INT): INT = INT(g.reflect("+",x,y.x))
    def -(y: INT): INT = INT(g.reflect("-",x,y.x))
    def *(y: INT): INT = INT(g.reflect("*",x,y.x))
  }

  def IF(c: INT)(a: => INT)(b: => INT): INT = {
    // TODO effect polymorphism!
    INT(g.reflect("?",c.x,g.reify(a.x),g.reify(b.x)))
  }
  
  def APP(f: Exp, x: INT): INT = 
    INT(g.reflect("@",f,x.x, g.curBlock)) // NOTE: control dep!

  def FUN(f: ((INT=>INT),INT) => INT): INT => INT = {
    val fn = Sym(g.fresh)
    //val xn = Sym(g.fresh)
    val f1 = (x: INT) => APP(fn,x)
    g.reflect(fn,"λ",g.reify(xn => f(f1,INT(xn)).x))
    f1
  }


  implicit def liftInt(x: Int): INT = INT(Const(x))


  def program(body: INT => INT): Graph = {
    val block = g.reify { arg => body(INT(arg)).x }
    Graph(g.globalDefs, block)
  }


}





class Example {



  def test(): Unit = {

    val f = new FrontEnd
    import f._

    var g = program { x =>
      val fac = FUN { (f, n) => 
        IF (n) {
          n * f(n-((2:INT)-1))
        } {
          1
        }
      }
      fac(x)
    }

    println("// Raw:")
    g.nodes.foreach(println)

    val dce = new DeadCodeElim

    g = dce(g)


    val flow = new Flow

    g = flow(g)


    println("// Generic Codegen:")
    (new CodeGen)(g)

    val codegen = new ScalaCodeGen {}

    println("// Scala Codegen:")
    codegen(g)

    val sc = new internal.ScalaCompile {}

    val src = utils.captureOut((new ScalaCodeGen)(g))
    val arg = g.block.in.head

    val fc = sc.compile[Int,Int] { cn =>
      s"class $cn extends (Int => Int) { def apply($arg: Int): Int = { $src } }"
    }

    println(fc(4))

    // Infrastructure TODOs:
    // - proper test cases
    //   - code motion
    //   - mutual recursion: ackermann 
    // - types
    // - compact printer (--> regalloc?)


    // DONE: cost-based code motion

    // TODO: liveness and compact printer

    // TODO: mutual recursion

    // TODO: fine-grained effects

    // TODO: lms tutorials & more ...

    // TODO: low-level:
    // - CPS conversion, 
    // - closure conversion
    // - x86 assembly
  }

}