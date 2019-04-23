package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

class CodeGen extends Traverser {

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

class ScalaCodeGen extends Traverser {

  def emit(s: String) = println(s)

  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x: String) => "\""+x+"\""
    case Const(x: Char) => "'"+x+"'"
    case Const(x) => x.toString
  }

  override def traverse(ns: Seq[Node], y: Block): Unit = {
    super.traverse(ns, y)
    emit(quote(y.res))
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block),_) =>
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int = {")
      // see what becomes available given new bound vars
      traverse(y, f)
      emit(s"}")
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit(s"val $f = if (${quote(c)}) {")
      traverse(a)
      emit(s"} else {")
      traverse(b)
      emit(s"}")
    case n @ Node(f,"W",List(c:Block,b:Block,e),_) =>
      emit(s"while ({")
      traverse(c)
      emit(s"}) else {")
      traverse(b)
      emit(s"}")
    case n @ Node(s,"+",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} + ${quote(y)}")
    case n @ Node(s,"-",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} - ${quote(y)}")
    case n @ Node(s,"*",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} * ${quote(y)}")
    case n @ Node(s,"/",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} / ${quote(y)}")
    case n @ Node(s,"%",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} % ${quote(y)}")
    case n @ Node(s,"==",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} == ${quote(y)}")
    case n @ Node(s,"!=",List(x,y),_) =>
      emit(s"val $s = ${quote(x)} != ${quote(y)}")
    case n @ Node(s,"var_new",List(x),_) =>
      emit(s"var $s = ${quote(x)}")
    case n @ Node(s,"var_get",List(x),_) =>
      emit(s"val $s = ${quote(x)}")
    case n @ Node(s,"var_set",List(x,y),_) =>
      emit(s"${quote(x)} = ${quote(y)}")
    case n @ Node(s,"array_new",List(x),_) =>
      emit(s"var $s = new Array[Int](${quote(x)})")
    case n @ Node(s,"array_get",List(x,i),_) =>
      emit(s"val $s = ${quote(x)}(${quote(i)})")
    case n @ Node(s,"array_set",List(x,i,y),_) =>
      emit(s"${quote(x)}(${quote(i)}) = ${quote(y)}")
    case n @ Node(s,"@",x::y::_,_) =>
      emit(s"val $s = ${quote(x)}(${quote(y)})")
    case n @ Node(s,"P",List(x),_) =>
      emit(s"val $s = println(${quote(x)})")
    case n @ Node(_,_,_,_) =>
      emit(s"??? " + n.toString)
  }
}

class CompactScalaCodeGen extends CompactTraverser {

  val rename = new mutable.HashMap[Sym,String]

  var doRename = false
  var doPrintEffects = false

  def emit(s: String): Unit = print(s)
  def emit(buf: ByteArrayOutputStream): Unit = buf.writeTo(Console.out)
  def emitln(s: String = "") = println(s)

  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(x: String) => "\""+x+"\""
    case Const(x: Char) => "'"+x+"'"
    case Const(x) => x.toString
    // case Eff(x) => x.map(quote).mkString("")
  }

  def capture(f: => Unit) = {
    val buf = new ByteArrayOutputStream()
    val bufWriter = new PrintStream(buf)
    utils.withOutput(bufWriter)(f)
    buf
  }

  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)
    case b:Block => quoteBlock1(b)
    case _ => emit(quote(n))
  }

  def quoteEff(x: Def): String =
    if (!doPrintEffects) ""
    else " /* " + quote(x) + " */"

  def quoteEff(x: List[Exp]): String =
    if (!doPrintEffects) ""
    else " /* " + x.map(quote).mkString("") + " */"

  def quoteEff(x: EffectSummary): String = quoteEff(x.deps)

  def quoteEff(n: Node): String = if (!doPrintEffects) "" else {
    val deps = n.eff.deps
    val eff = n.eff.keys
    if (deps.isEmpty && eff.isEmpty) "" else {
      s"/* val ${quote(n.n)} = ${eff.map(quote).mkString(",")}:${deps.map(quote).mkString("")} */"
    }
  }

  // def quoteEff(x: Effect) =
  //   if (!doPrintEffects || x.isEmpty) ""
  //   else " /* " + quote(x) + " */"

  var blockHeader: String = ""
  var currentPrec: Option[Int] = None
  def quoteBlock(f: => Unit): Unit = quoteBlockHelper("", None)(f)
  def quoteBlock(header: String)(f: => Unit): Unit = quoteBlockHelper(header, None)(f)
  def quoteBlockP(f: => Unit): Unit = quoteBlockHelper("", Some(0))(f)
  def quoteBlock1(y: Block, argType: Boolean = false) = {
    def eff = quoteEff(y.ein)
    if (y.in.length == 0) {
      quoteBlock(traverse(y))
      // if (b contains '\n')
        // s"{$eff\n" + eff + b + "\n}"
      // else
      //  b
    } else if (y.in.length == 1) {
      val x = y.in.head
      def typed(s:String) = if (argType) s+": Int" else s //FIXME hardcoded
      def paren(s:String) = if (argType) "("+s+")" else s
      quoteBlock(s"${paren(typed(quote(x)))}$eff => ")(traverse(y))
    } else (???) //FIXME
  }

  def quoteBlockHelper(header: String, prec: Option[Int])(f: => Unit) = {
    val save = blockHeader
    val save1 = currentPrec
    blockHeader = header
    currentPrec = prec
    f
    blockHeader = save
    currentPrec = save1
  }


  // process and print block results
  // TODO handle precedence in in scala correctly
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    // val paren = numStms == 0 && currentPrec.map(_ > 0).getOrElse(true)
    if (numStms > 0) {
      emit("{ "); emitln(blockHeader)
    } else {
      // if (paren) emit("(")
      emit(blockHeader)
    }
    super.traverseCompact(ns, y)
    if (y.res != Const(())) {
      shallow(y.res); emit(quoteEff(y.eff))
    } else {
      emit(quoteEff(y.eff))
    }
    if (numStms > 0) {
      emit("\n}")
    // } else {
    //   if (paren) emit(")")
    }
  }

  def precendence(n: Node) = n match {
    case _ => 0
  }

  def shallow1(n: Def, prec: Int = 20): Unit = n match {
    case InlineSym(n) if n.op != "var_get" && precendence(n) < prec => emit("("); shallow(n); emit(")")
    case _ => shallow(n)
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  def shallow(n: Node): Unit = { n match {
    case n @ Node(f,"λ",List(y:Block),_) =>
      // XXX what should we do for functions?
      // proper inlining will likely work better
      // as a separate phase b/c it may trigger
      // further optimizations
      quoteBlock1(y, true)
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit("if ("); shallow(c); emit(") ")
      quoteBlockP(traverse(a))
      emit(" else ")
      quoteBlockP(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit("while ("); quoteBlockP(traverse(c)); emit(") ")
      quoteBlock(traverse(b))
    case n @ Node(s,"+",List(x,y),_) =>
      shallow(x); emit(" + "); shallow(y)
    case n @ Node(s,"-",List(x,y),_) =>
      shallow(x); emit(" - "); shallow(y)
    case n @ Node(s,"*",List(x,y),_) =>
      shallow(x); emit(" * "); shallow(y)
    case n @ Node(s,"/",List(x,y),_) =>
      shallow(x); emit(" / "); shallow(y)
    case n @ Node(s,"%",List(x,y),_) =>
      shallow(x); emit(" % "); shallow(y)
    case n @ Node(s,"==",List(x,y),_) =>
      shallow(x); emit(" == "); shallow(y)
    case n @ Node(s,"!=",List(x,y),_) =>
      shallow(x); emit(" != "); shallow(y)
    case n @ Node(s,"<=",List(x,y),_) =>
      shallow(x); emit(" <= "); shallow(y)
    case n @ Node(s,">=",List(x,y),_) =>
      shallow(x); emit(" >= "); shallow(y)
    case n @ Node(s,"var_get",List(x),_) =>
      shallow(x)
    case n @ Node(s,"array_get",List(x,i),_) =>
      shallow(x); emit("("); shallow(i); emit(")")
    case n @ Node(s,"@",x::y,_) =>
      shallow1(x); emit("("); y.headOption.foreach(h => { shallow1(h, 0); y.tail.foreach(a => { emit(", "); shallow1(a, 0) }) }); emit(")")
    case n @ Node(s,"P",List(x),_) =>
      emit("println("); shallow(x); emit(")")
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      quoteBlock {
        emitln("//#" + str)
        if (verbose) {
          emitln("// generated code for " + str.replace('_', ' '))
        } else {
          emitln("// generated code")
        }
        traverse(b)
        emitln("//#" + str)
      }

    case n @ Node(_,op,args,_) =>
      emit(op); emit("("); args.headOption.foreach(h => { shallow1(h, 0); args.tail.foreach(a => { emit(", "); shallow1(a, 0) }) }); emit(")")
  }; emit(quoteEff(n)) }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block),_) =>
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int${quoteEff(y.ein)} = "); quoteBlockP(traverse(y,f)); emitln("")
    // XXX: should not need these below!
    case n @ Node(s,"P",_,_) => // Unit result
      shallow(n); emitln("")
    case n @ Node(s,"W",_,_) => // Unit result
      shallow(n); emitln("")
    case n @ Node(s,"var_new",List(x),_) =>
      emit(s"var ${quote(s)} = "); shallow(x); emitln("")
    case n @ Node(s,"var_set",List(x,y),_) =>
      emit(s"${quote(x)} = "); shallow(y); emitln("")
    case n @ Node(s,"array_new",List(x),_) =>
      emit(s"val ${quote(s)} = new Array[Int]("); shallow(x); emitln(")")
    case n @ Node(s,"array_set",List(x,i,y),_) =>
      shallow(x); emit("("); shallow(i); emit(") = "); shallow(y); emitln("")
    case _ =>
      // emit(s"val ${quote(s)} = " + shallow(n))
      emitValDef(n)
  }

  def emitValDef(n: Node): Unit = {
    emit(s"val ${quote(n.n)} = "); shallow(n); emitln("")
  }

  override def apply(g: Graph): Unit = {
    super.apply(g)
    emitln("")
  }
}

class DeadCodeElimCG {

  var live: collection.Set[Sym] = _
  var reach: collection.Set[Sym] = _

  def valueSyms(n: Node): List[Sym] =  n match {
    case n @ Node(s, "?", List(Block(_,res:Sym,_,_),(a:Block),(b:Block)), _) if !live(s) => res :: directSyms(n)
    case n @ Node(s, "?", List(c,(a:Block),(b:Block)), _) if !live(s) => directSyms(n)
    case _ => directSyms(n) ++ blocks(n).collect { case Block(_,res:Sym,_,_) => res }
  }

  def mysyms(n: Node): Set[Sym] = n match {
    case n @ Node(s, "?", List(c,(a:Block),(b:Block)), _) if !live(s) =>
      val (s, e) = symsAndEffectSyms(n)
      (s -- List(a.res, b.res).collect { case s: Sym => s}) ++ e
    case _ => syms(n).toSet
  }

  // staticData -- not really a DCE task, but hey
  var statics: collection.Set[Node] = _

  def apply(g: Graph): Unit = {

    live = new mutable.HashSet[Sym]
    reach = new mutable.HashSet[Sym]
    statics = new mutable.HashSet[Node]

    reach ++= g.block.used

    if (g.block.res.isInstanceOf[Sym])
      live += g.block.res.asInstanceOf[Sym]

    for (d <- g.nodes.reverseIterator) {
      if (reach(d.n)) {
        live ++= valueSyms(d)
        reach ++= mysyms(d)
        if (d.op == "staticData")
          statics += d
      }
    }

    //Graph(g.nodes.filter(d => live(d.n)), g.block)
  }
}

trait ExtendedCodeGen extends CompactTraverser {

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

  val dce = new DeadCodeElimCG

  var stream: java.io.PrintStream = _

  def quote(s: Def): String
  def remap(m: Manifest[_]): String

  def init(g: Graph): Unit = { dce(g) }


  def quoteStatic(n: Node) = n match {
    case Node(s, "staticData", List(Const(a)), _) =>
      val arg = "p"+quote(s)
      val tpe = a match { // FIXME: hardcoded ...
        case a: Array[Array[Int]] => "Array[Array[Int]]"
        case a: Array[Int] => "Array[Int]"
        case a: Array[Array[Double]] => "Array[Array[Double]]"
        case a: Array[Double] => "Array[Double]"
        case a: Array[Array[Float]] => "Array[Array[Float]]"
        case a: Array[Float] => "Array[Float]"
        case a: Array[Array[Long]] => "Array[Array[Long]]"
        case a: Array[Long] => "Array[Long]"
      }
      s"$arg: $tpe"
  }

  def extractStatic(n: Node) = n match {
    case Node(s, "staticData", List(Const(a)), _) =>
      (a.getClass, a)
  }

  def extractAllStatics() = dce.statics.toList.map(extractStatic)

  def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit

}


class ExtendedScalaCodeGen extends ExtendedCodeGen {

  val rename = new mutable.HashMap[Sym,String]


  var doRename = true
  var doPrintEffects = false

  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n","\\n").replace("\t","\\t")+"\"" // TODO: more escapes?
    case Const(x) if x.isInstanceOf[Char] && x == '\n' => "'\\n'"
    case Const(x) if x.isInstanceOf[Char] && x == '\t' => "'\\t'"
    case Const(x) if x.isInstanceOf[Char] && x == 0    => "'\\0'"
    case Const(x) if x.isInstanceOf[Char] => s"'${x.asInstanceOf[Char]}'"
    case Const(x) if x.isInstanceOf[Long] => x.toString + "L"
    case Const(null) => "null"
    case Const(x) => x.toString
    // case Eff(x) => x.map(quote).mkString("")
  }

  def quoteEff(x: Def) = ""

  def remap(m: Manifest[_]): String = m.toString
  val nameMap: Map[String, String] = Map( // FIXME: tutorial-specific
    "ScannerNew"     -> "new scala.lms.tutorial.Scanner",
    "ScannerHasNext" -> "Scanner.hasNext",
    "ScannerNext"    -> "Scanner.next",
    "ScannerClose"   -> "Scanner.close",
    "ObjHashCode"    -> "Object.hashCode",
    "DFAState"       -> "new scala.lms.tutorial.Automaton[Char,Boolean]"
  )


  def shallow1(n: Def): Unit = n match {
    case InlineSym(n) if n.op != "var_get" => emit("("); shallow(n); emit(")")
    case _ => shallow(n)
  }


  var recursive = false

  // process and print block results
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    // if (numStms > 0) emit("{\n")

    // Are there any forward nodes? if yes, declare all variables before
    // assigning initial values to avoid "forward reference extends
    // over definition" errors. Exception: lambdas emitted as defs.
    val save = recursive
    recursive = ns.seq.exists(_.op == "λforward")

    if (recursive) {
      for (d <- ns if d.op != "λ" && shouldInline(d.n).isEmpty && dce.live(d.n)) {
        emit("var "); emit(quote(d.n)); emit(": "); emit(remap(typeMap(d.n)))
        emitln(s" = null.asInstanceOf[${remap(typeMap(d.n))}]")
      }
    }

    super.traverseCompact(ns, y)
    if (y.res != Const(())) {
      shallow(y.res); emitln()
    }
    // if (numStms > 0) emit("\n}")
    recursive = save
  }

  def quoteBlockp(x: => Unit) {
    emitln("{")
    x
    emit("}")
  }

  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)
    case b:Block => quoteBlockp(traverse(b)) // FIXME: lambda args
    case _ => emit(quote(n))
  }


  val binop = Set("+","-","*","/","%","==","!=","<",">",">=","<=","&","|","<<",">>","Boolean.&&","Boolean.||") // TODO merge with CompactScalaCodeGen
  val scalaMath = Set("sin", "cos", "tanh", "exp", "sqrt")
  val numTypeConv = Set("toInt", "toLong", "toFloat", "toDouble")

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  def shallow(n: Node): Unit = n match {
    case n @ Node(f,"λforward",List(y),_) => emit(quote(y))
    case n @ Node(f,"λ",List(y:Block),_) =>
      // XXX what should we do for functions?
      // proper inlining will likely work better
      // as a separate phase b/c it may trigger
      // further optimizations
      // quoteBlock1(y, true)
      quoteBlockp(traverse(y))
    case n @ Node(s,"?",List(c,a,b:Block),_) if b.isPure && b.res == Const(false) =>
      shallow1(c); emit(" && "); shallow1(a)
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit(s"if ("); shallow(c); emit(") ")
      quoteBlockp(traverse(a))
      emit(s" else ")
      quoteBlockp(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit(s"while (")
      quoteBlockp(traverse(c))
      emit(s") ")
      quoteBlockp(traverse(b))
    case n @ Node(s,"var_get",List(x),_) =>
      shallow(x)
    case n @ Node(s,"array_get",List(x,i),_) =>
      shallow1(x); emit("("); shallow(i); emit(")")
    case n @ Node(s, "NewArray" ,List(x), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit("new Array["); emit(tpe); emit("]("); shallow(x); emit(")")
    case n @ Node(s,"@",x::y,_) => {
      def emitArgs(y: List[Def]): Unit = y match {
        case t::Nil => shallow(t)
        case t::ys  => shallow(t); emit(","); emitArgs(ys)
      }
      shallow1(x); emit("("); emitArgs(y); emit(")")
    }
    case n @ Node(s,"P",List(x),_) =>
      emit("println"); emit("("); shallow(x); emit(")")
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      quoteBlockp {
        emitln("//#" + str)
        if (verbose) {
          emitln("// generated code for " + str.replace('_', ' '))
        } else {
          emitln("// generated code")
        }
        traverse(b)
        emitln("//#" + str)
      }

    case n @ Node(s,"Boolean.!",List(a),_) =>
      emit("!"); shallow1(a)

    case n @ Node(s,"staticData",List(Const(a)),_) =>
      val q = a match { case x: Array[_] => "Array("+x.mkString(",")+")" case _ => a }
      emit("p"+quote(s)); emit(s" /* staticData $q */")

    case n @ Node(s,op,args,_) if nameMap contains op =>
      shallow(n.copy(op = nameMap(n.op)))

    case n @ Node(s,op,List(x,y),_) if binop(op) =>
      shallow1(x); emit(" "); emit(op); emit(" "); shallow1(y)

    case n @ Node(s,op,List(x),_) if scalaMath(op) =>
      emit(s"scala.math.$op("); shallow1(x); emit(")")

    // case n @ Node(s,op,List(x),_) if numTypeConv(op) =>
    //   shallow1(x); emit(s".$op")
    case n @ Node(s, "cast", List(a), _) =>
      val tpe = remap(typeMap.getOrElse(s, manifest[Unknown]))
      shallow1(a); emit(s".to$tpe")

    case n @ Node(s,op,args,_) if op.startsWith("unchecked") => // unchecked
      var next = 9 // skip unchecked
      for (a <- args) {
        val i = op.indexOf("[ ]", next)
        assert(i >= next)
        emit(op.substring(next,i))
        shallow(a)
        next = i + 3
      }
      emit(op.substring(next))


    case n @ Node(s,op,args,_) if op.contains('.') && !op.contains(' ') => // method call
      val (recv::args1) = args
      shallow1(recv); emit("."); emit(op.drop(op.lastIndexOf('.')+1))
      if (args1.nonEmpty) {
        emit("(")
        shallow(args1.head)
        args1.tail.foreach { a =>
          emit(", "); shallow(a)
        }
        emit(")")
      }

    case n @ Node(_,op,args,_) =>
      emit(s"$op(");
      if (args.nonEmpty) {
        shallow(args.head)
        args.tail.foreach { a =>
          emit(", "); shallow(a)
        }
      }
      emit(")")
  }


  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "exit", List(x) ,_) =>
      emit("System.exit("); shallow(x); emitln(")")
    case n @ Node(f,"λ",List(y:Block),_) =>
      val x = y.in
      val a = x.map(typeMap(_))
      val b = typeMap.getOrElse(y.res, manifest[Unit])
      val e = quoteEff(y.ein)

      val args = (x zip a).map{case (x, a) => s"${quote(x)}:$a"}.mkString(", ")
      emit(s"def ${quote(f)}($args): $b$e = ")
      quoteBlockp(traverse(y,f))
      emitln()

    case n @ Node(s,"generate-comment",List(Const(x)),_) =>
      emit("// "); emitln(x.toString)

    case n @ Node(s,"var_new",List(x),_) =>
      /*if (dce.live(s))*/
      if (!recursive) {
        emit(s"var ${quote(s)} = "); shallow(x); emitln()
      } else {
        emit(s"${quote(s)} = "); shallow(x); emitln()
      }
    case n @ Node(s,"var_set",List(x,y),_) =>
      emit(s"${quote(x)} = "); shallow(y); emitln()
    case n @ Node(s,"array_new",List(x),_) =>
      /*if (dce.live(s))*/ emit(s"val ${quote(s)} = new Array[Int]("); shallow(x); emit(")"); emitln()
    case n @ Node(s,"array_set",List(x,i,y),_) =>
      shallow(x); emit("("); shallow(i); emit(") = "); shallow(y); emitln()

    case n @ Node(s,"var_get",_,_) if !dce.live(s) => // no-op
    case n @ Node(s,"array_get",_,_) if !dce.live(s) => // no-op

    case n @ Node(s,_,_,_) =>
      if (!recursive) {
        if (dce.live(s)) emit(s"val ${quote(s)} = "); shallow(n); emitln()
      } else {
        if (dce.live(s)) emit(s"${quote(s)} = "); shallow(n); emitln()
    }
  }

  def emit(s: String) = stream.print(s)
  def emitln(s: String) = stream.println(s)
  def emitln() = stream.println()


  def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    init(g)
    val arg = quote(g.block.in.head)
    val efs = "" //quoteEff(g.block.ein)
    val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
    val (ms1, ms2) = (remap(m1), remap(m2))
    val className = name
    stream.println("""
    /*****************************************
    Emitting Generated Code
    *******************************************/
    """)
    stream.println(s"class $className($stt) extends ($ms1 => $ms2) {")
    stream.println(s"  def apply($arg: $ms1): $ms2$efs = {")
    apply(g)
    stream.println( "  }")
    stream.println( "}")
    stream.println("""
    /*****************************************
    End of Generated Code
    *******************************************/
    """)
  }

}

abstract class ExtendedCodeGen1 extends CompactScalaCodeGen with ExtendedCodeGen {

  // var typeMap: collection.Map[lms.core.Backend.Exp, Manifest[_]] = _

  // val dce = new DeadCodeElim
  doRename = true

  override def quote(x: Def) = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n","\\n").replace("\t","\\t")+"\"" // TODO: more escapes?
    case Const(x) if x.isInstanceOf[Char] && x == '\n' => "'\\n'"
    case Const(x) if x.isInstanceOf[Char] && x == '\t' => "'\\t'"
    case Const(x) if x.isInstanceOf[Char] && x == 0    => "'\\0'"
    case Const(x) if x.isInstanceOf[Long] => x.toString + "L"
    case Const(x: List[_]) => "{" + x.mkString(", ") + "}" // translate a Scala List literal to C List literal
    case _ => super.quote(x)
  }

  // The following table lists the precedence and associativity of C operators. Operators are listed
  // top to bottom, in descending precedence. (https://en.cppreference.com/w/c/language/operator_precedence)
  // Precedence Operator  Description Associativity
  // 1 | ++ --       | Suffix/postfix increment and decrement|  Left-to-right
  //   | ()          | Function call
  //   | []          | Array subscripting
  //   | .           | Structure and union member access
  //   | ->          | Structure and union member access through pointer
  //   | (type){list}| Compound literal(C99)
  //
  // 2 | ++ --       | Prefix increment and decrement[note 1]|  Right-to-left
  //   | + -         | Unary plus and minus
  //   | ! ~         | Logical NOT and bitwise NOT
  //   | (type)      | Type cast
  //   | *           | Indirection (dereference)
  //   | &           | Address-of
  //   | sizeof      | Size-of[note 2]
  //   | _Alignof    | Alignment requirement(C11)
  //
  // 3 | * / %       | Multiplication, division, and remainder|  Left-to-right
  //
  // 4 | + -         | Addition and subtraction
  //
  // 5 | << >>       | Bitwise left shift and right shift
  //
  // 6 | < <=        | For relational operators < and ≤ respectively
  //   | > >=        | For relational operators > and ≥ respectively
  //
  // 7 | == !=       | For relational = and ≠ respectively
  //
  // 8 | &           | Bitwise AND
  //
  // 9 | ^           | Bitwise XOR (exclusive or)
  //
  // 10| |           | Bitwise OR (inclusive or)
  //
  // 11| &&          | Logical AND
  //
  // 12| ||          | Logical OR
  //
  // 13| ?:          | Ternary conditional[note 3]|  Right-to-Left
  //
  // 14| =           | Simple assignment
  //   | += -=       | Assignment by sum and difference
  //   | *= /= %=    | Assignment by product, quotient, and remainder
  //   | <<= >>=     | Assignment by bitwise left shift and right shift
  //   | &= ^= |=    | Assignment by bitwise AND, XOR, and OR
  //
  // 15|,            | Comma|  Left-to-right


  // TODO: should make it language specific and move it down to ExtendedCCode
  def precedence(n: Node): Int = n match {
    case Node(s,"?",List(c,a:Block,b:Block),_) if b.isPure && b.res == Const(false) => precedence("&&")
    case Node(s,"?",List(c,a:Block,b:Block),_) if a.isPure && a.res == Const(true) => precedence("||")
    case _ => precedence(n.op)
  }
  final def unaryPrecedence(op: String): Int = 12
  final def precedence(op: String): Int = op match {
    case "?" => 1
    case "||" => 2
    case "&&" => 3
    case "|" => 4
    case "^" => 5
    case "&" => 6
    case "==" | "!=" | "String.equalsTo" => 7
    case "<" | ">" | "<=" | ">=" => 8
    case "<<" | ">>" => 9
    case "+" | "-" => 10
    case "*" | "/" | "%" => 11
    case "ref_new" => 13 // & address of
    case "reffield_get" => 14 // -> pointer member access
    // type casting is lower in precedence?
    case "NewArray" => 19 // there might be type conversion before array access, which should be put in parenthesis
    case _ if op.startsWith("unchecked") => 0 // force parenthesis if nested: 3 * unchecked(5 + 4)
    case _ => 20
  }

  // XXX proper operator precedence
  override def shallow1(n: Def, pp: Int = 20): Unit = n match {
    case InlineSym(n) if precedence(n) < pp => emit("("); shallow(n); emit(")")
    case _ => shallow(n)
  }
  var generateReturn = false
  // block of statements
  override def quoteBlock(f: => Unit) = quoteBlockHelp(None, false)(f)
  //  block of statements with result expression
  def quoteBlockPReturn(f: => Unit) = quoteBlockHelp(None, true)(f)
  override def quoteBlockP(f: => Unit) = quoteBlockHelp(Some(0), false)(f)
  def quoteBlockP(prec: Int)(f: => Unit) = quoteBlockHelp(Some(prec), false)(f)
  def quoteBlockHelp(prec: Option[Int], genRet: Boolean)(f: => Unit) = {
    val save = currentPrec
    val save1 = generateReturn
    currentPrec = prec
    generateReturn = genRet
    try f finally {
      currentPrec = save
      generateReturn = save1
    }
  }

  /*
   * Current precedence
   *     None => not used as value
   *     Some(n) => used in an expression, should have parenthesis if lastNode has lower precedence than n
   */
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    /* Generate parenthesis if block used as value (currentPrec != None) AND:
     *    - there is at least one statement
     *    - the precendence of the inlined node (lastNode) is less than the precedence of the node in which the block is generated (currentPrec)
     */
    val paren = currentPrec.map(prec => numStms > 0 || lastNode.map(n => { precedence(n) < prec}).getOrElse(false)).getOrElse(false)
    if (paren) emit("(")
    /* Generate braces:
     *  - if there is no stetement and no return value (empty block)
     *  - if there is at least one statement and a return value
     *  - if there are more than one statement
     *
     * n > 1                       n > 1                      n > 1
     * || n == 0 && res == () ==>  || n == 0 && res == () ==> || n == 0 && res == ()
     * || n > 0 && res != ()       || n == 1 && res != ()     || n != 0 && res != ()
     *
     * ==>
     *
     * n > 1 && (n == 0 & res != ())
     */
    val brace = generateReturn || numStms > 1 || (numStms == 0 ^ y.res != Const(()))
    if (brace) emitln("{")
    for (n <- ns) {
      if (shouldInline(n.n).isEmpty)
        traverse(n)
    }
    if (y.res != Const(())) {
      if (generateReturn) emit("return ");
      shallow(y.res); emit(quoteEff(y.eff));
    } else {
      emit(quoteEff(y.eff))
    }
    if (generateReturn && y.res != Const(()) || currentPrec != None && numStms > 0) emit(";")
    if (brace) emit("\n}")
    if (paren) emit(")")
  }

  def array(innerType: String): String
  def primitive(rawType: String): String
  def record(man: RefinedManifest[_]): String
  def function(sig: List[Manifest[_]]): String
  def remap(m: Manifest[_]): String = m.typeArguments match {
    case Nil => m match {
      case ref: RefinedManifest[_] => record(ref)
      case _ => primitive(m.toString)
    }
    case List(inner) => array(remap(inner))
    case sig => function(sig)
  }
  def nameMap: Map[String, String]

  override def emitValDef(n: Node): Unit = {
    if (dce.live(n.n))
      super.emitValDef(n)
    else {
      shallow(n)
    }
  }

  override def shallow(n: Def): Unit = n match {
    case InlineSym(t: Node) => shallow(t)
    case b:Block => quoteBlock1(b)
    case _ => emit(quote(n))
  }

  private val headers = mutable.HashSet[String]("<stdio.h>", "<stdlib.h>", "<stdint.h>","<stdbool.h>")
  def registerHeader(nHeaders: String*) = headers ++= nHeaders.toSet
  def emitHeaders(out: PrintStream) = headers.foreach { f => out.println(s"#include $f") }

  private val registeredFunctions = mutable.HashSet[String]()
  private val functionsStream = new ByteArrayOutputStream()
  private val functionsWriter = new PrintStream(functionsStream)
  private var ongoingFun = false
  def registerTopLevelFunction(id: String)(f: => Unit) = if (!registeredFunctions(id)) { // FIXME: Can't be nested!
    if (ongoingFun) ???
    ongoingFun = true
    registeredFunctions += id
    utils.withOutput(functionsWriter)(f)
    ongoingFun = false
  }
  def emitFunctions(out: PrintStream) = if (functionsStream.size > 0){
    out.println("\n/************* Functions **************/")
    functionsStream.writeTo(out)
  }

  private val registeredDatastructures = mutable.HashSet[String]()
  private val datastructuresStream = new ByteArrayOutputStream()
  private val datastructuresWriter = new PrintStream(datastructuresStream)
  private var ongoingData = false
  def registerDatastructures(id: String)(f: => Unit) = if (!registeredDatastructures(id)) {
    if (ongoingData) ???
    ongoingData = true
    registeredDatastructures += id
    utils.withOutput(datastructuresWriter)(f)
    ongoingData = false
  }
  def emitDatastructures(out: PrintStream) = if (datastructuresStream.size > 0) {
    out.println("\n/*********** Datastructures ***********/")
    datastructuresStream.writeTo(out)
  }

  val unaryop = Set("-","!","&")
  val binop = Set("+","-","*","/","%","==","!=","<",">",">=","<=","&","|","<<",">>", "&&", "||")
  override def shallow(n: Node): Unit = n match {
    case n @ Node(s,op,args,_) if nameMap contains op =>
      shallow(n.copy(op = nameMap(n.op)))
    case n @ Node(f, "λforward",List(y),_) => ??? // this case is short cut at traverse function!
    case n @ Node(s, op,List(x,y),_) if binop(op) => // associativity??
      shallow1(x, precedence(op)); emit(" "); emit(op); emit(" "); shallow1(y, precedence(op)+1)
    case n @ Node(s, op,List(x),_) if unaryop(op) => // associativity??
      emit(op); shallow1(x, unaryPrecedence(op))
    case n @ Node(s,"Boolean.!",List(a),_) =>
      emit("!"); shallow1(a, unaryPrecedence("!"))
    case n @ Node(s,op,args,_) if op.startsWith("unchecked") => // unchecked
      var next = 9 // skip unchecked
      for (a <- args) {
        val i = op.indexOf("[ ]", next)
        assert(i >= next)
        emit(op.substring(next,i))
        shallow(a)
        next = i + 3
      }
      emit(op.substring(next))
    case n @ Node(s,op,args,_) if op.startsWith("String") => // String methods
      registerHeader("<string.h>")
      (op.substring(7), args) match {
        case ("equalsTo", List(lhs, rhs)) => emit("strcmp("); shallow(lhs); emit(", "); shallow(rhs); emit(" == 0");
        case ("toDouble", List(rhs)) => emit("atof("); shallow(rhs); emit(")")
        case (a, _) => System.out.println(s"TODO: $a - ${args.length}"); ???
      }
    case n @ Node(s,op,args,_) if op.contains('.') && !op.contains(' ') => // method call
      val (recv::args1) = args
      if (args1.length > 0) {
        shallow1(recv); emit("."); emit(op.drop(op.lastIndexOf('.') + 1)); emit("("); shallow(args1.head); args1.tail.foreach { emit(", "); shallow(_) }; emit(")")
      } else {
        shallow1(recv); emit("."); emit(op.drop(op.lastIndexOf('.') + 1))
      }
    case n @ Node(s,"?",List(c,a,b:Block),_) if b.isPure && b.res == Const(false) => ??? // I think there are unused
      shallow1(c, precedence("&&")); emit(" && "); shallow1(a, precedence("&&")+1)
    case n @ Node(s,"?",List(c,a:Block,b),_) if a.isPure && a.res == Const(true) => ???
      shallow1(c, precedence("||")); emit(" || "); shallow1(b, precedence("||")+1)
    case n =>
      super.shallow(n)
  }
  override def traverse(n: Node): Unit = n match {
    case n @ Node(s,"var_get",_,_) if !dce.live(s) => // no-op
    case n @ Node(s,"array_get",_,_) if !dce.live(s) => // no-op
    case n @ Node(s,_,_,_) =>
      super.traverse(n)
  }

  // Needs to be C dependent
  // override def apply(g: Graph): Unit = {
  //   utils.withOutput(stream) {
  //     val ls = captureLines {
  //       bound(g)
  //       withScope(Nil, g.nodes) {
  //         traverse(g.block)
  //       }
  //     }
  //     if (g.block.res == Const(())) {
  //       ls.foreach(println)
  //     } else {
  //       ls.init.foreach(println)
  //       println(s"return ${ls.last};")
  //     }
  //   }
  // }
}

class Unknown // HACK: Sentinel for typeMap

class ExtendedCCodeGen extends ExtendedCodeGen1 {

  // Remap auxiliary function C specific
  def primitive(rawType: String): String = rawType match {
    case "Unit" => "void"
    case "Boolean" => "bool"
    case "Char" => "char"
    case "Int" => "int"
    case "Double" => "double"
    case "Float" => "float"
    case "Long" => "long"
    case "java.lang.String" => "char*"
    // case "Nothing" | "Any" => ???
    case _ => rawType
  }
  def array(innerType: String) = innerType + "*"
  def function(sig: List[Manifest[_]]): String = ???
  def record(man: RefinedManifest[_]): String = {
    val tpe = "struct " + man.toString
    registerDatastructures(tpe) {
      emit(tpe); emitln(" {")
      man.fields.foreach {
        case (name, man) => emitln(remap(man) + " " + name + ";")
      }
      emitln("};")
    }
    tpe
  }
  val nameMap = Map( // FIXME: tutorial specific
    "ScannerNew"     -> "new scala.lms.tutorial.Scanner",
    "ScannerHasNext" -> "Scanner.hasNext",
    "ScannerNext"    -> "Scanner.next",
    "ScannerClose"   -> "Scanner.close",
    "ObjHashCode"    -> "Object.hashCode",
    "StrSubHashCode" -> "hash"
  )

  def emitFunction(name: String, body: Block) = {
    val res = body.res
    val args = body.in
    emit(s"${remap(typeBlockRes(res))} $name(${args map(s => s"${remap(typeMap.getOrElse(s, manifest[Unknown]))} ${quote(s)}") mkString(", ")}) ")
    quoteBlockPReturn(traverse(body))
  }

  override def emitValDef(n: Node): Unit = {
    // emit(s"val ${quote(s)} = $rhs; // ${dce.reach(s)} ${dce.live(s)} ")
    if (dce.live(n.n)) {
      emit(s"${remap(typeMap.getOrElse(n.n, manifest[Unknown]))} ${quote(n.n)} = "); shallow(n); emitln(";")
    } else {
      shallow(n); emitln(";")
    }
  }
  def emitVarDef(n: Node): Unit = {
    // emit(s"var ${quote(s)} = $rhs; // ${dce.reach(s)} ${dce.live(s)} ")
    // TODO: enable dce for vars ... but currently getting unused expression warnings ...
    // if (dce.live(s))
      emit(s"${remap(typeMap.getOrElse(n.n, manifest[Unknown]))} ${quote(n.n)} = "); shallow(n.rhs.head); emitln(";")
    // else
      // emit(s"$rhs;")
  }

  def compatibleType(m: Manifest[_], n: Manifest[_]) = {
    m == manifest[Char] && (n == manifest[Long] || n == manifest[Int] || n == manifest[Short]) || m == manifest[Short] && (n == manifest[Long] || n == manifest[Int]) || m == manifest[Int] && n == manifest[Long]
    // || m == manifest[Float] && n == manifest[Double]
  }

  def convert[T:Manifest](x: Any): T = x.asInstanceOf[T]

  override def shallow(n: Node): Unit = n match {
    case Node(s, "cast", List(a), _) =>
      val tpe = typeMap.getOrElse(s, manifest[Unknown])
      a match {// FIXME should it be there?
        case Const(n) =>
          emit(quote(Const(convert(n)(tpe))))
        case v@Sym(_) if compatibleType(typeMap.getOrElse(v, manifest[Unknown]), tpe) =>
          shallow(a)
        case b@Block(_, res, _, _) if compatibleType(typeMap.getOrElse(res, manifest[Unknown]), tpe) =>
          shallow(a)
        case _ =>
          emit(s"(${remap(tpe)})"); shallow1(a)
      }
    case Node(s,"String.charAt",List(a,i),_) =>
      shallow1(a); emit("["); shallow(i); emit("]")
    case Node(s,"array_get",List(a,i),_) =>
      shallow1(a); emit("["); shallow(i); emit("]")
    // case Node(s,"var_get",List(a),_) =>
      // quote(a)+s"/*${quote(s)}*/"

    // case n @ Node(s,"comment",_,_) =>
      // s"(${super.shallow(n)})" // GNU C block expr
    case n @ Node(s,"?",List(c,a:Block,b:Block),_) if b.isPure && b.res == Const(false) =>
      shallow1(c, precedence("&&")); emit(" && "); quoteBlockP(precedence("&&") + 1)(traverse(a))
    case n @ Node(s,"?",List(c,a:Block,b:Block),_) if a.isPure && a.res == Const(true) =>
      shallow1(c, precedence("||")); emit(" || "); quoteBlockP(precedence("||") + 1)(traverse(b))
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      shallow1(c); emit(" ? ")
      quoteBlockP(precedence("?"))(traverse(a))
      emit(" : ")
      quoteBlockP(precedence("?"))(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit("while (");
      quoteBlockP(traverse(c))
      emit(") ")
      quoteBlock1(b)
      emitln()
    case n @ Node(s,"generate-comment",List(Const(x: String)),_) =>
      emit("// "); emitln(x)
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      ???
      quoteBlockP {
        emitln("//#" + str)
        if (verbose) {
          emitln("// generated code for " + str.replace('_', ' '))
        } else {
          emitln("// generated code")
        }
        traverse(b)
        emit("/*#" + str + "*/")
      }

    case n @ Node(s,"P",List(x),_) =>
      emit("""printf("%s\n", """); shallow(x); emit(")");
    case n @ Node(s, "λtop", List(block: Block), _) =>
      registerTopLevelFunction(quote(s)) {
        emitFunction(quote(s), block)
      }
      emit(quote(s))
    case n @ Node(s, "reffield_get", List(ptr, Const(field: String)), _) =>
      shallow1(ptr, precedence("reffield_get")); emit("->"); emit(field)
    case n @ Node(s, "ref_new", List(v), _) =>
      emit("&"); shallow1(v, precedence("ref_new"))
    case n @ Node(s, "NewArray" ,List(x), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"($tpe*)malloc("); shallow1(x, precedence("*")); emit(s" * sizeof($tpe))")
    case n @ Node(s,"array_new",List(x),_) =>
      emit(s"(int*)malloc("); shallow1(x, precedence("*")); emit(s" * sizeof(int))")
    case n @ Node(s,"array_sort",List(arr, len, arr2, comp),_) => // FIXME: not arr duplicated?
      emit("qsort("); shallow1(arr); emit(" ,"); shallow1(len); emit(", sizeof(*"); shallow(arr2); emit("), (__compar_fn_t)"); shallow(comp); emit(")")
    case n =>
      super.shallow(n)
  }
  override def traverse(n: Node): Unit = n match {
    case n @ Node(s,"var_new",List(x),_) =>
      emitVarDef(n)
    case n @ Node(s, "local_struct", Nil, _) =>
      val tpe = remap(typeMap.getOrElse(s, manifest[Unknown]))
      emitln(s"$tpe ${quote(s)} = { 0 };") // FIXME: uninitialized? or add it as argument?
    case n @ Node(s,"var_set",List(x,y),_) =>
      emit(s"${quote(x)} = "); shallow(y); emitln(";")
    case n @ Node(s, "reffield_set", List(ptr, Const(field: String), v), _) =>
      shallow1(ptr, precedence("reffield_get")); emit("->"); emit(field); emit(" = "); shallow(v); emitln(";")
    // static array
    case n @ Node(s, "Array" , xs,_) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"$tpe ${quote(s)}[${xs.length}] = { "); shallow(xs.head); xs.tail.foreach(x => {emit(", "); shallow(x)}); emitln(" };")

    // DCE: FIXME:
    // (a) check that args have no side-effects
    //      maybe this can be ensured by overriding InlineSym?
    // DCE: FIXME:
    // (a) check that args have no side-effects
    //      maybe this can be ensured by overriding InlineSym?
    //      should never inline a live expr into something that's not live ...
    // (b) dce above should anticipate that these will be eliminated
    //      do not register dependencies as live!
    case n @ Node(s,"var_get",_,_) if !dce.live(s) => // no-op
    case n @ Node(s,"array_get",_,_) if !dce.live(s) => // no-op

    case n @ Node(s,"array_set",List(x,i,y),_) =>
      shallow1(x); emit("["); shallow(i); emit("] = "); shallow(y); emitln(";")


    case n @ Node(s,"?",c::(a:Block)::(b:Block)::_,_) if !dce.live(s) =>
      emit(s"if ("); shallow(c); emit(") ")
      quoteBlock(traverse(a.copy(res = Const(()))))
      emit(" else ")
      quoteBlock(traverse(b.copy(res = Const(()))))
      emitln()

    case n @ Node(s,"W",List(c:Block,b:Block),_) if !dce.live(s) => // is it necessary? the while value will always be dead.
      emit("while (")
      quoteBlockP(traverse(c))
      emit(") ")
      quoteBlock(traverse(b))
      emitln()

    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      emitln("//# " + str)
      if (verbose) {
        emitln("// generated code for " + str.replace('_', ' '))
      } else {
        emitln("// generated code")
      }
      if (dce.live(s)) {
        val tpe = remap(typeMap.getOrElse(s, manifest[Unknown]))
        emit(tpe); emit(" "); emit(quote(s)); emit(" = "); quoteBlockP(traverse(b))
      } else {
        traverse(b)
      }
      emitln(";\n//# " + str)
    case n @ Node(s, "λforward", List(y), _) =>
      emitln("//# lambda forward is here!")
      // for this case, adding (f, y) in forwardMap is all we need
      // XXX: this is most likely not enough in the general case
      rename(s) = quote(y)
    case n @ Node(s, "λtop", List(block@Block(args, res, _, _)), _) => ??? // shouldn't be possible in C
    case n @ Node(s, "timestamp", _, _) =>
      registerHeader("<sys/time.h>")
      emitln(s"struct timeval ${quote(s)}_t;")
      emitln(s"gettimeofday(&${quote(s)}_t, NULL);")
      emitln(s"long ${quote(s)} = ${quote(s)}_t.tv_sec * 1000000L + ${quote(s)}_t.tv_usec;");
    case _ =>
      // emit(s"val ${quote(s)} = " + shallow(n))
      emitValDef(n)
  }

  def run(name: String, g: Graph) = {
    capture {
      bound(g)
      withScope(Nil, g.nodes) {
        emitFunction(name, g.block)
      }
    }
  }

  def convert(arg: String, man: Manifest[_]): String = {
    if (man == manifest[Int]) s"atoi($arg)"
    else if (man == manifest[String]) arg
    else "" // manifest[Unit]
  }

  def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    init(g)
    val efs = "" //quoteEff(g.block.ein)
    val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
    stream.println("""
    /*****************************************
    Emitting C Generated Code
    *******************************************/
    """)
    val src = run(name, g)
    emitHeaders(stream)
    emitDatastructures(stream)
    emitFunctions(stream)
    stream.println(s"\n/**************** $name ****************/")
    src.writeTo(stream)
    stream.println("""
    |/*****************************************
    |End of C Generated Code
    |*******************************************/
    |int main(int argc, char *argv[]) {
    |  if (argc != 2) {
    |    printf("usage: %s <arg>\n", argv[0]);
    |    return 0;
    |  }""".stripMargin)
    stream.println(s"  $name(${convert("argv[1]", m1)});\n  return 0;\n}")
  }
}
