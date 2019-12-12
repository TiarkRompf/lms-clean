package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._

class ScalaCodeGen extends Traverser {

  def emit(s: String) = println(s)

  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(x: String) => "\""+x+"\""
    case Const(x: Char) => "'"+x+"'"
    case Const(x) => x.toString
  }

  var printRes = true
  override def traverse(ns: Seq[Node], y: Block): Unit = {
    super.traverse(ns, y)
    if (printRes) emit(quote(y.res))
    else printRes = true
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f, "λ", (y:Block)::_, _) =>
      val para = y.in.length match {
        case 0 => ""
        case 1 => s"${quote(y.in.head)}: Int"
        case 2 => s"${quote(y.in.head)}: Int => Int, ${quote(y.in.tail.head)}: Int"
      }
      emit(s"def ${quote(f)}($para): Int = {")
      // see what becomes available given new bound vars
      traverse(y, f)
      emit(s"}")
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit(s"val $f = if (${quote(c)}) {")
      traverse(a)
      emit(s"} else {")
      traverse(b)
      emit(s"}")
    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit(s"while ({")
      traverse(c)
      emit(s"}) {")
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
    case n @ Node(s,"@",x::y,_) =>
      emit(s"val $s = ${quote(x)}(${y.map(quote).mkString(", ")})")
    case n @ Node(s,"P",List(x),_) =>
      emit(s"val $s = println(${quote(x)})")
    case n @ Node(s,">",List(x,y), _) =>
      emit(s"val $s = ${quote(x)} > ${quote(y)}")
    case n @ Node(s,"<",List(x,y), _) =>
      emit(s"val $s = ${quote(x)} < ${quote(y)}")
    case n @ Node(s,">=",List(x,y), _) =>
      emit(s"val $s = ${quote(x)} >= ${quote(y)}")
    case n @ Node(s,"<=",List(x,y), _) =>
      emit(s"val $s = ${quote(x)} <= ${quote(y)}")
    case n @ Node(s,"λforward",List(x), _) =>
      emit(s"lazy val $s = ${quote(x)} _")
    case n @ Node(s, "define_exit", _, _) =>
      emit(s"def exit(res: Int): Int = res")
    case n @ Node(s, "exit", List(x), _) =>
      emit(s"${quote(x)} /*exit: ${quote(x)} */")
      printRes = false
    case n @ Node(s, "reset0", List(x:Block), _) =>
      emit(s"val $s = {"); traverse(x); emit("\n}")
    case n @ Node(_,_,_,_) =>
      emit(s"??? " + n.toString)
  }

  override def apply(g: Graph): Unit = {
    bound(g)
    withScope(Nil, g.nodes) { traverse(g.block) }
  }

  def emitAll(g: Graph)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    val arg = quote(g.block.in.head)
    emit(
      s"""
        |class Snippet extends (${m1.toString} => ${m2.toString}) {
        |  def apply($arg: ${m1.toString}): ${m2.toString} = {
       """.stripMargin)
    apply(g)
    emit("\n}\n}")
  }
}

class CompactScalaCodeGen extends CompactTraverser {

  val rename = new mutable.HashMap[Sym,String]

  var doRename = true
  var doPrintEffects = false

  def emit(s: String): Unit = print(s)
  def emitln(s: String = ""): Unit = println(s)

  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n","\\n").replace("\t","\\t")+"\"" // TODO: more escapes?
    case Const(x: Char) if x == '\n' => "'\\n'"
    case Const(x: Char) if x == '\t' => "'\\t'"
    case Const(x: Char) if x == 0    => "'\\0'"
    case Const(x: Char) => "'"+x+"'"
    case Const(x: Long) => x.toString + "L"
    case Const(null) => "null"
    case Const(x) => x.toString
  }

  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)
    case b:Block => quoteBlock(b)
    case _ => emit(quote(n))
  }

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
  def withWraper(w: WrapFun)(f: => Unit) = {
    val save = wraper
    wraper = w
    f
    wraper = save // needed?
  }

  def nowraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = f
  var wraper: WrapFun = nowraper _

  def quoteBlock(f: => Unit): Unit = quoteBlock("")(f)
  def quoteBlock(header: String)(f: => Unit): Unit = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      // XXX(GW): for performance, can we omit those checks and just emit { } anyway?
      if (numStms > 0 || header.startsWith("case")) emitln("{ " + header)
      else emit(header)
      f
      if (y.res != Const(())) shallow(y.res)
      emit(quoteEff(y.eff))
      if (numStms > 0 || header.startsWith("case")) emit("\n}")
    }
    withWraper(wraper _)(f)
  }
  def quoteBlock(b: Block): Unit = {
    def eff = quoteEff(b.ein)
    if (b.in.length == 0) {
      quoteBlock(traverse(b)) //FIXME(GW): discard eff if arg length == 0?
    } else {
      val argsStr = b.in.map(arg => quote(arg)).mkString("(", ", ", s")$eff => ")
      quoteBlock(argsStr)(traverse(b))
    }
  }
  def quoteBlockP(f: => Unit): Unit = quoteBlockP(0)(f)
  def quoteBlockP(prec: Int)(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      val paren = numStms == 0 && l.map(n => precedence(n) < prec).getOrElse(false)
      if (paren) emit("(") else if (numStms > 0) emitln("{")
      f
      if (y.res != Const(())) { shallow(y.res); emit(quoteEff(y.eff)) } else emit(quoteEff(y.eff))
      if (paren) emit(")") else if (numStms > 0) emit("\n}")
    }
    withWraper(wraper _)(f)
  }
  def noquoteBlock(f: => Unit) = withWraper(nowraper _)(f)
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

  def precedence(n: Node): Int = n match {
    case Node(s,"?",List(c,a,b:Block),_) if b.isPure && b.res == Const(false) => precedence("&&")
    case Node(s,"?",List(c,a:Block,b),_) if a.isPure && a.res == Const(true) => precedence("||")
    case Node(s,op,List(x),_) if unaryop(op) => unaryPrecedence(op)
    case _ => precedence(n.op)
  }

  final def unaryPrecedence(op: String): Int = 12
  def precedence(op: String): Int = op match {
    case "?" => 1
    case "||" => 2
    case "&&" => 3
    case "|" => 4
    case "^" => 5
    case "&" => 6
    case "==" | "!=" => 7
    case "<" | ">" | "<=" | ">=" => 8
    case "<<" | ">>" | ">>>" => 9
    case "+" | "-" => 10
    case "*" | "/" | "%" => 11
    case "cast" => 12
    case _ if op.startsWith("unchecked") => 0 // force parenthesis if nested: 3 * unchecked(5 + 4)
    case "λ" => 19 // less than 20?
    case _ => 20
  }

  // process and print block results
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    wraper(numStms, lastNode, y) {
      super.traverseCompact(ns, y)
    }
  }

  def shallow1(n: Def, prec: Int = 20): Unit = n match {
    case InlineSym(n) if precedence(n) < prec => emit("("); shallow(n); emit(")")
    case _ => shallow(n)
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  val unaryop = Set("-","!","&")
  val binop = Set("+","-","*","/","%","==","!=","<",">",">=","<=","&","|","<<",">>", ">>>", "&&", "||", "^")
  val math = Set("sin", "cos", "tanh", "exp", "sqrt")
  def shallow(n: Node): Unit = { n match {
    case n @ Node(f, "λ", (y:Block)::_, _) =>
      // XXX what should we do for functions?
      // proper inlining will likely work better
      // as a separate phase b/c it may trigger
      // further optimizations
      // Note: if argument types have to be emitted, ExtendedScalaCodeGen has overrided this case
      quoteBlock(y)

    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit("if ("); shallow(c); emit(") ")
      quoteBlockP(traverse(a))
      emit(" else ")
      quoteBlockP(traverse(b))

    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit("while ("); quoteBlockP(traverse(c)); emit(") ")
      quoteBlock(traverse(b))

    case n @ Node(s, op,List(x,y),_) if binop(op) => // associativity??
      shallow1(x, precedence(op)); emit(" "); emit(op); emit(" "); shallow1(y, precedence(op)+1)

    case n @ Node(s, op,List(x),_) if unaryop(op) => // associativity??
      emit(op); shallow1(x, unaryPrecedence(op))

    case n @ Node(s,op,List(x),_) if math(op) =>
      emit(s"scala.math.$op("); shallow1(x); emit(")")

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
    case n @ Node(f, "λ", (y:Block)::_, _) =>
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

  override def apply(g: Graph) = {
    quoteBlockP(super.apply(g))
    emitln()
  }
}

trait Pattern
case class PVar(x: Sym) extends Pattern
case class PTuple(xs: List[Pattern]) extends Pattern

class ExtendedScalaCodeGen extends CompactScalaCodeGen with ExtendedCodeGen {
  // val rename = new mutable.HashMap[Sym,String]
  var lastNL = false
  override def emit(s: String): Unit = { stream.print(s); lastNL = false }
  override def emitln(s: String = "") = if (s != "" || !lastNL) { stream.println(s); lastNL = true }

  def array(innerType: String): String = ""
  def primitive(rawType: String): String = ""
  def record(man: RefinedManifest[_]): String = ""
  def function(sig: List[Manifest[_]]): String = ""
  def remapUnsigned(m: Manifest[_]) = ???
  override def remap(m: Manifest[_]): String = m.toString

  val nameMap: Map[String, String] = Map( // FIXME: tutorial-specific
    "ScannerNew"     -> "new scala.lms.tutorial.Scanner",
    "ScannerHasNext" -> "Scanner.hasNext",
    "ScannerNext"    -> "Scanner.next",
    "ScannerClose"   -> "Scanner.close",
    "ObjHashCode"    -> "Object.hashCode",
    "DFAState"       -> "new scala.lms.tutorial.Automaton[Char,Boolean]"
  )

  var recursive = false

  // process and print block results
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {

    // Are there any forward nodes? if yes, declare all variables before
    // assigning initial values to avoid "forward reference extends
    // over definition" errors. Exception: lambdas emitted as defs.
    val save = recursive
    recursive = ns.seq.exists(_.op == "λforward")
    val save1 = wraper
    if (recursive) {
      def nwraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
        save1(numStms, l, y) {
          emitln()
          for (d <- ns if d.op != "λ" && shouldInline(d.n).isEmpty && dce.live(d.n)) {
            emit("var "); emit(quote(d.n)); emit(": "); emit(remap(typeMap(d.n)))
            emitln(s" = null.asInstanceOf[${remap(typeMap(d.n))}]")
          }
          f
        }
      }
      wraper = nwraper _
    }
    super.traverseCompact(ns, y)
    recursive = save
    wraper = save1 // necessary?
  }

  def quoteBlock(b: Block, emitType: Boolean = false): Unit = {
    def eff = quoteEff(b.ein)
    if (b.in.length == 0) {
      quoteBlock(traverse(b)) //FIXME(GW): discard eff if arg length == 0?
    } else {
      val argsStr = b.in.map({ arg =>
        val tp = if (emitType) s": ${remap(typeMap.getOrElse(arg, manifest[Unknown]))}" else ""
        quote(arg) + tp
      }).mkString("(", ", ", s")$eff => ")
      quoteBlock(argsStr)(traverse(b))
    }
  }

  def patternToString(p: Pattern, emitType: Boolean = false): String = p match {
    case PVar(x) =>
      val tp = if (emitType) s": ${remap(typeMap.getOrElse(x, manifest[Unknown]))}" else ""
      quote(x) + tp
    case PTuple(xs) => "(" + xs.map(patternToString(_)).mkString(", ") + ")"
  }
  
  //TODO(GW): merge this code into exisitng quoteBlock or reify?
  // The current block.in is a special case of a tuple of arguments, can be generalized.
  // More generally, a block can have multiple cases.
  def quoteCaseBlock(b: Block, argPattern: Pattern, emitType: Boolean = false): Unit = {
    val patStr = patternToString(argPattern, emitType)
    quoteBlock("case " + patStr + " =>")(traverse(b))
  }

  def shallow(n: Def, emitType: Boolean = false): Unit = n match {
    case b:Block => quoteBlock(b, emitType)
    case _ => super.shallow(n)
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  override def shallow(n: Node): Unit = n match {
    case n @ Node(f,"λforward",List(y),_) => emit(quote(y))
    case n @ Node(f, "λ", (y:Block)::_, _) =>
      // XXX what should we do for functions?
      // proper inlining will likely work better
      // as a separate phase b/c it may trigger
      // further optimizations
      /*
      val argsTyped = y.in map { arg => s"${quote(arg)}: ${remap(typeMap.getOrElse(arg, manifest[Unknown]))}" }
      val argsTypedStr = argsTyped.mkString("(", ",", ") => ")
      quoteBlock(argsTypedStr)(traverse(y))
      */
      quoteBlock(y, true)
    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if b.isPure && b.res == Const(false) =>
      shallow1(c, precedence("&&")); emit(" && "); quoteBlockP(precedence("&&") + 1)(traverse(a))
    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if a.isPure && a.res == Const(true) =>
      shallow1(c, precedence("||")); emit(" || "); quoteBlockP(precedence("||") + 1)(traverse(b))
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      emit(s"if ("); shallow(c); emit(") ")
      quoteBlockP(traverse(a))
      quoteElseBlock(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      emit(s"while (")
      quoteBlockP(traverse(c))
      emit(s") ")
      quoteBlock(traverse(b))
    case n @ Node(s,"var_get",List(x),_) =>
      shallow(x)
    case n @ Node(s,"array_get",List(x,i),_) =>
      shallow1(x); emit("("); shallow(i); emit(")")
    case n @ Node(s,"array_length",List(x), _) =>
      shallow(x); emit(".length")
    case n @ Node(s,"array_free", List(arr), _) => ()
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
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) => ??? // Comment shouldn't be inlined
      emitln("//# " + str)
      if (verbose) {
        emitln("// generated code for " + str.replace('_', ' '))
      } else {
        emitln("// generated code")
      }
      quoteBlockP {
        traverse(b)
      }
      emitln("\n//# " + str)

    case n @ Node(s,"staticData",List(Const(a)),_) =>
      val q = a match { case x: Array[_] => "Array("+x.mkString(",")+")" case _ => a }
      emit("p"+quote(s)); emit(s" /* staticData $q */")

    case n @ Node(s,op,args,_) if nameMap contains op =>
      shallow(n.copy(op = nameMap(n.op)))

    // case n @ Node(s,op,List(x),_) if numTypeConv(op) =>
    //   shallow1(x); emit(s".$op")
    case n @ Node(s, "cast", a::_, _) =>
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
    case n @ Node(s, "timestamp", _, _) => emitln(s"System.nanoTime / 1000L")
    case n @ Node(s, "array_sort_scala", List(arr, l), _) => shallow1(arr); emit(".slice(0, "); shallow1(l); emitln(").sorted")

    case _ => super.shallow(n)
  }

  override def emitValDef(n: Node): Unit = {
    if (!recursive) {
      if (dce.live(n.n)) emit(s"val ${quote(n.n)} = ");
      shallow(n); emitln()
    } else {
      if (dce.live(n.n)) emit(s"${quote(n.n)} = ");
      shallow(n); emitln()
    }
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "exit", List(x), _) =>
      emit("System.exit("); shallow(x); emitln(")")
    case n @ Node(f, "λ", (y: Block)::_, _) =>
      val args = y.in
      val types = args.map { a => remap(typeMap.getOrElse(a, manifest[Unknown])) }
      val retType = remap(typeMap.getOrElse(y.res, manifest[Unit]))
      val eff = quoteEff(y.ein)

      val argsStr = (args zip types).map{ case (x, a) => s"${quote(x)}: $a" }.mkString(", ")
      emit(s"def ${quote(f)}($argsStr): $retType$eff = ")
      quoteBlockP(traverse(y,f))
      emitln()

    case n @ Node(s,"generate-comment",List(Const(x)),_) =>
      emit("// "); emitln(x.toString)

    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      emitln("//# " + str)
      if (verbose) {
        emitln("// generated code for " + str.replace('_', ' '))
      } else {
        emitln("// generated code")
      }
      if (dce.live(s)) {
        emit("val "); emit(quote(s)); emit(" = "); quoteBlockP(traverse(b))
      } else {
        noquoteBlock(traverse(b))
      }
      emitln("\n//# " + str)
    case n @ Node(_, "switch", guard::default::others, _) =>
      shallow1(guard); emitln(" match {");
      others.grouped(2).foreach {
        case List(Const(cases: Seq[Const]), block: Block) =>
          emit("case "); emit(quote(cases.head)); cases.tail.foreach(x => emit(s" | ${quote(x)}")); emitln(" =>")
          noquoteBlock(traverse(block))
      }
      default match {
        case block: Block =>
          emitln("case _ =>")
          noquoteBlock(traverse(block))
        case _ =>
      }
      emitln("}")
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

    case n @ Node(s,"var_get",_,_) if !dce.live(s) => ??? // no-op
    case n @ Node(s,"array_get",_,_) if !dce.live(s) => ??? // no-op

    case n =>
      emitValDef(n)
  }

  override def apply(g: Graph): Unit = {
    bound(g)
    withScope(Nil, g.nodes) {
      quoteBlockP(traverse(g.block))
      emitln()
    }
  }

  def emitAll(g: Graph, name: String)(m1: Manifest[_], m2: Manifest[_]): Unit = {
    val ng = init(g)
    val arg = quote(g.block.in.head)
    val efs = "" //quoteEff(g.block.ein)
    val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
    val (ms1, ms2) = (remap(m1), remap(m2))
    val className = name
    emitln("""
    /*****************************************
    Emitting Generated Code
    *******************************************/
    """)
    emitln(s"class $className($stt) extends ($ms1 => $ms2) {")
    emit(s"  def apply($arg: $ms1): $ms2$efs = ")
    quoteBlock(apply(ng))
    emitln("}")
    emitln("""
    /*****************************************
    End of Generated Code
    *******************************************/
    """)
  }
}
