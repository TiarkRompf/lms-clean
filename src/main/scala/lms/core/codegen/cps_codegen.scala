package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

class CPSScalaCodeGen extends CPSTraverser {

  var trace = 0
  def fresh = try { trace } finally { trace += 1 }
  def emit(s: String) = print(s)
  def emitln(s: String) = println(s)
  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x: String) => "\""+x+"\""
    case Const(x: Char) => "'"+x+"'"
    case Const(x) => x.toString
  }

  val contSet = mutable.Set.empty[Exp]

  override def traverse(n: Node)(k: => Unit): Unit = n match {

    case n @ Node(s,"shift1",List(y:Block),_) =>
      contSet += y.in.head
      emitln(s"def ${quote(y.in.head)}($s: Int) = {"); k; emitln("\n}")
      traverse(y)(v => emitln(quote(v)))

    case n @ Node(s,"reset1",List(y:Block),_) =>
      emitln(s"val ${quote(s)} = {")
      traverse(y){ v => emit(quote(v)) }
      emitln("\n}")
      k

    case n @ Node(f,"λ",List(y:Block),_) =>
      val x = y.in.head
      emitln(s"def ${quote(f)}(c: Int => Int, ${quote(x)}: Int): Int = {")
      traverse(y, f){ v => emit("c("); emit(quote(v)); emit(")") }
      emitln("\n}")
      k

    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      val index = fresh
      emitln(s"def cIf$index($f: Int) = {"); k; emitln("\n}")
      emitln(s"if (${quote(c)}) {")
      traverse(a){ v => emit(s"cIf$index("); emit(quote(v)); emit(")") }
      emitln("\n} else {")
      traverse(b){ v => emit(s"cIf$index("); emit(quote(v)); emit(")") }
      emitln("\n}")

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

    case n @ Node(s,"@",x::y::_,_) =>
      if (x.isInstanceOf[Sym] && contSet.contains(x.asInstanceOf[Sym])) {
        emitln(s"val ${quote(s)} = ${quote(x)}(${quote(y)})"); k
      }
      else {
        val index = fresh
        emitln(s"def cApp$index($s: Int) = {"); k; emitln("\n}")
        emitln(s"${quote(x)}(cApp$index, ${quote(y)})")
      }

    case n @ Node(s,"P",List(x),_) =>
      emitln(s"val $s = println(${quote(x)})"); k
    case n @ Node(s,">",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} > ${quote(y)}"); k
    case n @ Node(s,"<",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} < ${quote(y)}"); k
    case n @ Node(s,">=",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} >= ${quote(y)}"); k
    case n @ Node(s,"<=",List(x,y), _) =>
      emitln(s"val $s = ${quote(x)} <= ${quote(y)}"); k
    case n @ Node(s,"λforward",List(x), _) =>
      emitln(s"lazy val $s = ${quote(x)} _"); k
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

