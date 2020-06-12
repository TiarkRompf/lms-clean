package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

class Unknown // HACK: Sentinel for typeMap

/**
 * This class demonstrates a minimal code generator from Traverser.
 * It is not used in production.
 */
class GenericCodeGen extends Traverser {

  def emit(s: String) = println(s)

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    emit(s"// in: ${y.in.mkString(" ")} effect: ${y.ein}") // XXX compat
    super.traverse(ns, y)
    emit(y.res.toString + " // out effect: " + y.eff.toString)
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f, "λ", List(y: Block), _) =>
      emit(s"$f = (λ {")
      traverse(y, f)
      emit(s"})")
    case n @ Node(f, op, es, eff) =>
      val ss = es map {
        case e @ Block(_,_,_,_) => "{" + utils.captureOut(traverse(e)) + "}" // FIXME freq!!
        case e => e.toString
      }
      val efc = if (eff.deps.nonEmpty) s" // Eff: ${eff}" else ""
      emit(s"$f = ($op ${ss.mkString(" ")})$efc")
  }
}


trait ExtendedCodeGen {

  // Method Group 1: stream and emit (for codegen string handling)
  var stream: java.io.PrintStream = _
  def withStream(out: PrintStream)(f: => Unit) = {
    val save = stream
    stream = out
    f
    stream = save
  }
  def capture(f: => Unit) = {
    val buf = new ByteArrayOutputStream()
    val bufWriter = new PrintStream(buf)
    withStream(bufWriter)(f)
    buf
  }

  def emit(buf: ByteArrayOutputStream): Unit = buf.writeTo(stream)

  // Method Group 2: typeMap (from core Exp to type Manifest, the information
  //                 of which is collection in the frontend)
  var typeMap: collection.Map[lms.core.Backend.Exp, Manifest[_]] = _
  def typeBlockRes(x: lms.core.Backend.Exp) = x match {
    case Const(()) => manifest[Unit]
    case Const(x: Int) => manifest[Int]
    case Const(x: Long) => manifest[Long]
    case Const(x: Float) => manifest[Float]
    case Const(x: Double) => manifest[Double]
    case Const(x: Char) => manifest[Char]
    case Const(x: String) => manifest[String]
    case Const(_) => ???
    case _ => typeMap.getOrElse(x, manifest[Unknown])
  }

  // Method Group 3: remap (from type Manifest to string representation of type)
  def array(innerType: String): String
  def primitive(rawType: String): String
  def record(man: RefinedManifest[_]): String
  def function(sig: List[Manifest[_]]): String
  def remapUnsigned(m: Manifest[_]): String
  def remap(m: Manifest[_]): String = m.typeArguments match {
    case Nil => m match {
      case ref: RefinedManifest[_] => record(ref)
      case _ => primitive(m.toString)
    }
    case List(inner) => array(remap(inner))
    case sig => function(sig)
  }

  // Method Group 4: dead code elimination
  val dce = new DeadCodeElimCG
  def init(g: Graph): Graph = dce(g)

  // Method Group 5: quote (for generating string of a unit of code generation:
  //                 variable, const, or block)
  def quote(s: Def): String
  def quoteStatic(n: Node) = n match {
    case Node(s, "staticData", List(Const(a)), _) =>
      val arg = "p"+quote(s)
      val tpe = remap(typeMap.getOrElse(s, manifest[Unknown]))
      s"$arg: $tpe"
  }

  def extractStatic(n: Node) = n match {
    case Node(s, "staticData", List(Const(a)), _) =>
      (a.getClass, a)
  }

  def extractAllStatics() = dce.statics.toList.map(extractStatic)

  // Method Group 6: nameMap (from Node Op string to Target Language Method string)
  def nameMap: Map[String, String]

  // Head Function: emitAll take a graph and generate the code.
  def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit
}
