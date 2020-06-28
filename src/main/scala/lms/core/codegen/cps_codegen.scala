package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

/**
 * On top of the CPSTraverser, we can build code generators that run in CPS.
 * Here we show a CPSScalaCodeGen. However, the CPSScalaCodeGen isn't just generating
 * normal code with CPS traverser. It is generated code that can manipulate continuations:
 * i.e., support `shift1` and `reset1` in the LMS IR.
 *
 * However, note that this code gen is quite rudimentary (no inlining, types are simple).
 * This is just a show case of CPS code gen, not the one actually used in production.
 */
class CPSScalaCodeGen extends CPSTraverser {

  var trace = 0
  def fresh = try { trace } finally { trace += 1 }
  def emit(s: String) = print(s)
  def emitln(s: String) = println(s)

  // Although the paramete `s` is of type `Def`
  // this function is only handling `Exp`
  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x: String) => "\""+x+"\""
    case Const(x: Char) => "'"+x+"'"
    case Const(x) => x.toString
    case _ => System.out.println("Block not supported"); ???
  }

  // this function tracks the set of continuations, because
  // application of continutations do not generate another continuation
  val contSet = mutable.Set.empty[Exp]

  // In this simple case, the main difference between a codegen pass and
  // a generic traverser is that each node has to generate some code.
  override def traverse(n: Node)(k: => Unit): Unit = n match {

    // `shift` captures the current continutation (generate a def)
    // and then generates the `shift` body.
    case n @ Node(s,"shift1",List(y:Block),_) =>
      contSet += y.in.head
      emitln(s"def ${quote(y.in.head)}($s: Int) = {"); k; emitln("\n}")
      traverse(y)(v => emitln(quote(v)))

    // `reset` needs to deliminate the continuation, thus the continuation `k` is
    // not supplied to the traverse of the reset block.
    case n @ Node(s,"reset1",List(y:Block),_) =>
      emitln(s"val ${quote(s)} = {")
      traverse(y){ v => emit(quote(v)) }
      emitln("\n}")
      k

    // In CPS code, all functions should have additional parameter, the continuation
    case n @ Node(f,"λ",List(y:Block),_) =>
      val x = y.in.head
      emitln(s"def ${quote(f)}(c: Int => Int, ${quote(x)}: Int): Int = {")
      traverse(y, f){ v => emit("c("); emit(quote(v)); emit(")") }
      emitln("\n}")
      k

    // "λforward" is the used for recursive functions (as a function name declaration)
    case n @ Node(s,"λforward",List(x, arity), _) =>
      emitln(s"lazy val $s = ${quote(x)} _"); k

    // application needs to be split into 2 cases: function appliation and continuation application
    // Continuation Application (the true case) is simple (just apply and continue)
    // Function Application (the false case) should capture the continuation
    //    and use it as the first argument of the generated function.
    case n @ Node(s,"@",x::y::_,_) =>
      if (x.isInstanceOf[Sym] && contSet.contains(x.asInstanceOf[Sym])) {
        emitln(s"val ${quote(s)} = ${quote(x)}(${quote(y)})"); k
      } else {
        val index = fresh
        emitln(s"def cApp$index($s: Int) = {"); k; emitln("\n}")
        emitln(s"${quote(x)}(cApp$index, ${quote(y)})")
      }

    // In CPS conditional, there should be a continuation `cIf`, and then
    // each branch should call the continuation at the end.
    // Note that the 2 traverse(block) cannot be code in tandem
    // like `traverse(a){ v => emit ... ; traverse(b) { v => emit ...}}`
    // because we are doing code generation (the generated code are static, and
    // both branches are generated). For instance, if the true branch has a
    // `shift1`, the code for the false branch should not be captured as part of
    // the shift continuation!
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      val index = fresh
      emitln(s"def cIf$index($f: Int) = {"); k; emitln("\n}")
      emitln(s"if (${quote(c)}) {")
      traverse(a){ v => emit(s"cIf$index("); emit(quote(v)); emit(")") }
      emitln("\n} else {")
      traverse(b){ v => emit(s"cIf$index("); emit(quote(v)); emit(")") }
      emitln("\n}")

    // CPS loop is generated as recursive function `def loop`
    // In the recursive function, the true branch recurses on `loop`, while
    // the false branch runs the code after the loop (the continuation).
    case n @ Node(f,"W", List(c:Block, b:Block),_) =>
      val index = fresh
      emitln(s"def loop$index(): Int = {")
      traverse(c){ v => emit("if ("); emit(quote(v)); emit(") ") }
      emitln("{")
      traverse(b) { v => emit(s"loop$index()") }
      emitln("\n} else {")
      k
      emitln("\n}\n}")
      emit(s"loop$index()")

    // Simple computations are generated in direct style.
    case n @ Node(s,"+",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} + ${quote(y)}"); k
    case n @ Node(s,"-",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} - ${quote(y)}"); k
    case n @ Node(s,"*",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} * ${quote(y)}"); k
    case n @ Node(s,"/",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} / ${quote(y)}"); k
    case n @ Node(s,"%",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} % ${quote(y)}"); k
    case n @ Node(s,"==",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} == ${quote(y)}"); k
    case n @ Node(s,"!=",List(x,y),_) =>
      emitln(s"val $s = ${quote(x)} != ${quote(y)}"); k
    case n @ Node(s,">",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} > ${quote(y)}"); k
    case n @ Node(s,"<",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} < ${quote(y)}"); k
    case n @ Node(s,">=",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} >= ${quote(y)}"); k
    case n @ Node(s,"<=",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} <= ${quote(y)}"); k
    case n @ Node(s,"var_new",List(x),_) =>
      emitln(s"var $s = ${quote(x)}"); k
    case n @ Node(s,"var_get",List(x),_) =>
      emitln(s"val $s = ${quote(x)}"); k
    case n @ Node(s,"var_set",List(x,y),_) =>
      emitln(s"${quote(x)} = ${quote(y)}"); k
    case n @ Node(s,"array_new",List(x),_) =>
      emitln(s"var $s = new Array[Int](${quote(x)})"); k
    case n @ Node(s,"array_get",List(x,i),_) =>
      emitln(s"val $s = ${quote(x)}(${quote(i)})"); k
    case n @ Node(s,"array_set",List(x,i,y),_) =>
      emitln(s"${quote(x)}(${quote(i)}) = ${quote(y)}"); k
    case n @ Node(s,"P",List(x),_) =>
      emitln(s"val $s = println(${quote(x)})"); k
    case n @ Node(_,_,_,_) =>
      emitln(s"??? " + n.toString); k
  }

  override def apply(g: Graph): Unit = {
    bound(g)
    path = Nil; inner = g.nodes
    traverse(g.block){ e => emit(s"${quote(e)} /*exit ${quote(e)}*/") }
  }

  def emitAll(g: Graph)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    val arg = quote(g.block.in.head)
    emitln(
      s"""
        |class Snippet extends (${m1.toString} => ${m2.toString}) {
        |  def apply($arg: ${m1.toString}): ${m2.toString} = {
       """.stripMargin)
    apply(g)
    emitln("\n}\n}")
  }
}

