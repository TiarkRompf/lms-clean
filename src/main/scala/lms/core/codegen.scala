package lms.core

import lms.macros.{RefinedManifest, CustomManifest}

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

class Unknown // HACK: Sentinel for typeMap

/**
 * This `GenericCodeGen` class demonstrates a minimal code generator from Traverser.
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

/**
 * This `ExtendedCodeGen` trait defines several interfaces for all Extend*CodeGen,
 * where * refers to a target language such as Scala, C, CPP, Rust.
 * 1. Interface for code gen stream, emit, emitln.
 * 2. Interface for typeMap (from backend.Exp to Manifest).
 * 3. Interface for remap (from Manifest to String representation of the type).
 * 4. Interface for dead code elimination (DCE).
 * 5. Interface for quote* (code gen of Exp, Block, and static data).
 * 5* Interface for shallow* (code gen of a target langauge right-hand-side)
 * 5* Interface for traverse* (code gen of a target language statement)
 * 6. Interface for nameMap (from IR Op string to target language function).
 * 7. Interface for code gen (emitAll).
 */
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
      case cus: CustomManifest[_] => m.toString
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


abstract class CompactCodeGen extends CompactTraverser {

  // process and print block results
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    wraper(numStms, lastNode, y) {
      super.traverseCompact(ns, y)
    }
  }
  // the `emit` and `emitln` are basic codegen units.
  // the actual desitination of them are determined by children classes.
  def emit(s: String): Unit
  def emitln(s: String = ""): Unit

  /**
   * `quote` family of methods are used to emit a codegen unit (a variable, constant, or block)
   * We have `quote` for variable/const
   *         `quoteEff` for effects
   *         `quoteBlock` `quoteBlockP` `quoteElseBlock` for
   *          block,    block with precedence, and else block
   */
  // with `rename` `doRename` and the usage of them in `quote`, we may
  // rename variables (say from x6 to x4).
  val rename = new mutable.HashMap[Sym,String]
  var doRename = true
  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n","\\n").replace("\t","\\t")+"\"" // TODO: more escapes?
    case Const(x: Char) if x == '\n' => "'\\n'"
    case Const(x: Char) if x == '\t' => "'\\t'"
    case Const(x: Char) if x == 0    => "'\\0'"
    case Const(x: Char) => "'"+x+"'"
    case Const(x: Long) => x.toString + "L"
    case Const(x) => x.toString
  }

  // with `doPrintEffects` and `quoteEff`, we can optionally print the effect information
  var doPrintEffects = false
  def quoteEff(x: Def): String =
    if (!doPrintEffects) ""
    else " /* " + quote(x) + " */"

  def quoteEff(x: Set[Sym]): String =
    if (!doPrintEffects) ""
    else " /* " + x.toSeq.sorted.map(quote).mkString(" ") + " */"

  def quoteEff(x: EffectSummary): String = quoteEff(x.deps)

  def quoteEff(n: Node): String = if (!doPrintEffects) "" else {
    if (n.eff.isEmpty) "" else {
      s"/* val ${quote(n.n)} = ${n.eff.repr(quote)} */"
    }
  }

  type WrapFun = (Int, Option[Node], Block) => (=> Unit) =>Unit
  var wraper: WrapFun = nowraper _
  def withWraper(w: WrapFun)(f: => Unit) = {
    val save = wraper
    wraper = w
    f
    wraper = save // needed?
  }

  def nowraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = f
  def noquoteBlock(f: => Unit) = withWraper(nowraper _)(f)

  def quoteBlock(f: => Unit): Unit = quoteBlock("")(f)
  def quoteBlock(header: String)(f: => Unit): Unit
  def quoteBlock(b: Block, argType: Boolean = false): Unit

  val unaryop = Set("-","!","&")
  val binop = Set("+","-","*","/","%","==","!=","<",">",">=","<=","&","|","<<",">>", ">>>", "&&", "||", "^")
  val math = Set("sin", "cos", "tanh", "exp", "sqrt")
  final def unaryPrecedence(op: String): Int = 12
  def precedence(op: String): Int
  def precedence(n: Node): Int = n match {
    case Node(s,"?",List(c,a,b:Block),_) if b.isPure && b.res == Const(false) => precedence("&&")
    case Node(s,"?",List(c,a:Block,b),_) if a.isPure && a.res == Const(true) => precedence("||")
    case Node(s,op,List(x),_) if unaryop(op) => unaryPrecedence(op)
    case _ => precedence(n.op)
  }

  def quoteBlockP(f: => Unit): Unit = quoteBlockP(0)(f)
  def quoteBlockP(prec: Int)(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      val paren = numStms == 0 && l.map(n => precedence(n) < prec).getOrElse(false)
      if (paren) emit("(") else if (numStms > 0) emitln("{")
      f
      if (y.res != Const(())) { shallow(y.res) }
      emit(quoteEff(y.eff))
      if (paren) emit(")") else if (numStms > 0) emit("\n}")
    }
    withWraper(wraper _)(f)
  }

  def quoteElseBlock(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      if (numStms > 0) {
        emitln(" else {")
        f
        if (y.res != Const(())) shallow(y.res)
        emitln(quoteEff(y.eff))
        emit("}")
      } else {
        if (y.res != Const(())) { emit(" else "); shallow(y.res) }
        emit(quoteEff(y.eff))
      }
    }
    withWraper(wraper _)(f)
  }

  /**
   * The `shallow` family of codegen methods handle `Def`s that are in the RHS of target langauge.
   * They are different from `traverse` because they don't generate a variable bindings for the nodes.
   */
  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)
    case b:Block => quoteBlock(b)
    case _ => emit(quote(n))
  }

  def shallowP(n: Def, prec: Int = 20): Unit = n match {
    case InlineSym(n) if precedence(n) < prec => emit("("); shallow(n); emit(")")
    case _ => shallow(n)
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  def shallow(n: Node): Unit = n match {
    case n @ Node(s, op,List(x,y),_) if binop(op) => // associativity??
      shallowP(x, precedence(op)); emit(" "); emit(op); emit(" "); shallowP(y, precedence(op)+1)

    case n @ Node(s, op,List(x),_) if unaryop(op) => // associativity??
      emit(op); shallowP(x, unaryPrecedence(op))

    case n @ Node(s,"var_get",List(x),_) =>
      shallow(x)

    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit("if ("); shallow(c); emit(") ")
      quoteBlockP(traverse(a))
      emit(" else ")
      quoteBlockP(traverse(b))

    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit("while ("); quoteBlockP(traverse(c)); emit(") ")
      quoteBlock(traverse(b))

    case n @ Node(s,"@",x::y,_) =>
      shallowP(x); emit("("); y.headOption.foreach(h => { shallowP(h, 0); y.tail.foreach(a => { emit(", "); shallowP(a, 0) }) }); emit(")")

    case n @ Node(_,op,args,_) =>
      emit(op); emit("("); args.headOption.foreach(h => { shallowP(h, 0); args.tail.foreach(a => { emit(", "); shallowP(a, 0) }) }); emit(")")
  }
}