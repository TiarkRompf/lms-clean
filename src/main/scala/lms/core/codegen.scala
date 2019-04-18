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

  var lines: List[String] = Nil
  def emit(s: String) = if (s != "") lines = s::lines

  def quote(s: Def): String = s match {
    case s @ Sym(n) if doRename => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Sym(n) => s.toString
    case Const(x: String) => "\""+x+"\""
    case Const(x: Char) => "'"+x+"'"
    case Const(x) => x.toString
    // case Eff(x) => x.map(quote).mkString("")
  }

  def shallow(n: Def): String = n match {
    case InlineSym(n) => shallow(n)
    case b:Block => quoteBlock1(b)
    case _ => quote(n)
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


  // XXX NOTE: performance implications of capture + concat !!!
  // XXX TODO: what if block is empty?
  def captureLines(f: => Unit): List[String] = {
    val save = lines
    try {
      lines = Nil
      f
      lines.reverse
    } finally { lines = save }
  }

  def quoteBlock(f: => Unit) = {
    val ls = captureLines(f)
    if (ls.length != 1) {
      "{\n" + ls.mkString("\n") + "\n}"
    } else ls.mkString("\n")
  }
  def quoteBlock1(y: Block, argType: Boolean = false) = {
    def eff = quoteEff(y.ein)
    if (y.in.length == 0) {
      val b = quoteBlock(traverse(y))
      // if (b contains '\n')
        // s"{$eff\n" + eff + b + "\n}"
      // else
        b
    } else if (y.in.length == 1) {
      val x = y.in.head
      val l = captureLines(traverse(y))
      val b = l.mkString("\n")
      def typed(s:String) = if (argType) s+": Int" else s //FIXME hardcoded
      def paren(s:String) = if (argType) "("+s+")" else s
      if (l.length != 1)
        paren(s"{ ${typed(quote(x))}$eff => \n$b\n}")
      else
        s"(${paren(typed(quote(x)))}$eff => $b)"
    } else (???) //FIXME
  }


  // process and print block results
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    // if (numStms > 0) emit("{")
    super.traverseCompact(ns, y)
    if (y.res != Const(()))
      emit(shallow(y.res) + quoteEff(y.eff))
    else
      emit(quoteEff(y.eff))
    // if (numStms > 0) emit("}")
  }

  // generate string for node's right-hand-size
  // (either inline or as part of val def)
  // XXX TODO: precedence of nested expressions!!
  def shallow(n: Node): String = (n match {
    case n @ Node(f,"λ",List(y:Block),_) =>
      // XXX what should we do for functions?
      // proper inlining will likely work better
      // as a separate phase b/c it may trigger
      // further optimizations
      quoteBlock1(y, true)
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      s"if (${shallow(c)}) " +
      quoteBlock(traverse(a)) +
      s" else " +
      quoteBlock(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      s"while (" +
      quoteBlock(traverse(c)) +
      s") " +
      quoteBlock(traverse(b))
    case n @ Node(s,"+",List(x,y),_) =>
      s"${shallow(x)} + ${shallow(y)}"
    case n @ Node(s,"-",List(x,y),_) =>
      s"${shallow(x)} - ${shallow(y)}"
    case n @ Node(s,"*",List(x,y),_) =>
      s"${shallow(x)} * ${shallow(y)}"
    case n @ Node(s,"/",List(x,y),_) =>
      s"${shallow(x)} / ${shallow(y)}"
    case n @ Node(s,"%",List(x,y),_) =>
      s"${shallow(x)} % ${shallow(y)}"
    case n @ Node(s,"==",List(x,y),_) =>
      s"${shallow(x)} == ${shallow(y)}"
    case n @ Node(s,"!=",List(x,y),_) =>
      s"${shallow(x)} != ${shallow(y)}"
    case n @ Node(s,"<=",List(x,y),_) =>
      s"${shallow(x)} <= ${shallow(y)}"
    case n @ Node(s,">=",List(x,y),_) =>
      s"${shallow(x)} >= ${shallow(y)}"
    case n @ Node(s,"var_get",List(x),_) =>
      s"${shallow(x)}"
    case n @ Node(s,"array_get",List(x,i),_) =>
      s"${shallow(x)}(${shallow(i)})"
    case n @ Node(s,"@",x::y,_) =>
      s"${shallow(x)}(${y.map(shallow(_)).mkString(", ")})"
    case n @ Node(s,"P",List(x),_) =>
      s"println(${shallow(x)})"
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      quoteBlock {
        emit("//#" + str)
        if (verbose) {
          emit("// generated code for " + str.replace('_', ' '))
        } else {
          emit("// generated code")
        }
        traverse(b)
        emit("//#" + str)
      }

    case n @ Node(_,op,args,_) =>
      s"$op(${args.map(shallow).mkString(", ")})"
  }) + quoteEff(n)

  override def traverse(n: Node): Unit = n match {
    case n @ Node(f,"λ",List(y:Block),_) =>
      val x = y.in.head
      emit(s"def ${quote(f)}(${quote(x)}: Int): Int${quoteEff(y.ein)} = ${ quoteBlock(traverse(y,f)) }")
    // XXX: should not need these below!
    case n @ Node(s,"P",_,_) => // Unit result
      emit(shallow(n))
    case n @ Node(s,"W",_,_) => // Unit result
      emit(shallow(n))
    case n @ Node(s,"var_new",List(x),_) =>
      emit(s"var ${quote(s)} = ${shallow(x)}")
    case n @ Node(s,"var_set",List(x,y),_) =>
      emit(s"${quote(x)} = ${shallow(y)}")
    case n @ Node(s,"array_new",List(x),_) =>
      emit(s"val ${quote(s)} = new Array[Int](${shallow(x)})")
    case n @ Node(s,"array_set",List(x,i,y),_) =>
      emit(s"${shallow(x)}(${shallow(i)}) = ${shallow(y)}")
    case n @ Node(s,_,_,_) =>
      // emit(s"val ${quote(s)} = " + shallow(n))
      emitValDef(s, shallow(n))
  }

  def emitValDef(s: Sym, rhs: => String): Unit = {
    emit(s"val ${quote(s)} = " + rhs)
  }



  override def apply(g: Graph) = {
    val ls = captureLines(super.apply(g))
    ls.foreach(println)
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

    case n @ Node(s,op,List(x),_) if numTypeConv(op) =>
      shallow1(x); emit(s".$op")

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
      /*if (dce.live(s))*/ emit(s"val ${quote(s)} = "); emit("new Array[Int]("); shallow(x); emit(")"); emitln()
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
  val forwardMap = new scala.collection.mutable.HashMap[Sym,Def]()

  override def quote(x: Def) = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n","\\n").replace("\t","\\t")+"\"" // TODO: more escapes?
    case Const(x) if x.isInstanceOf[Char] && x == '\n' => "'\\n'"
    case Const(x) if x.isInstanceOf[Char] && x == '\t' => "'\\t'"
    case Const(x) if x.isInstanceOf[Char] && x == 0    => "'\\0'"
    case Const(x) if x.isInstanceOf[Long] => x.toString + "L"
    case Const(x: List[_]) => "{" + x.mkString(", ") + "}" // translate a Scala List literal to C List literal
    case a: Sym if forwardMap.contains(a) => quote(forwardMap(a))
    case _ => super.quote(x)
  }

  def isAtom(op: String) = !binop.contains(op)
  def precedence(op: String): Int = op match {
    case "Boolean.||" | "?" => 1 // node: in case "?" is rewritten to "&&" or "||"
    case "Boolean.&&" => 2
    case "|" => 3
    case "^" => 4
    case "&" => 5
    case "==" | "!=" | "String.equalsTo" => 6
    case "<" | ">" | "<=" | ">=" => 7
    case "<<" | ">>" => 8
    case "+" | "-" => 9
    case "*" | "/" | "%" => 10
    // type casting is lower in precedence?
    case "NewArray" => 19 // there might be type conversion before array access, which should be put in parenthesis
    case _ if op.startsWith("unchecked") => 0 // force parenthesis if nested: 3 * unchecked(5 + 4)
    case b if isAtom(b) => 20
    case _ => 0
  }

  // XXX proper operator precedence
  def shallow1(n: Def, pp: Int = 20): String = n match {
    case InlineSym(n) if (precedence(n.op) < pp) => s"(${shallow(n)})"
    case _ => shallow(n)
  }

  def array(innerType: String): String
  def primitive(rawType: String): String
  def record(man: RefinedManifest[_]): String
  def remap(m: Manifest[_]): String = m.typeArguments match {
    case Nil => m match {
      case ref: RefinedManifest[_] => record(ref)
      case _ => primitive(m.toString)
    }
    case List(inner) => array(remap(inner) )
  }
  def nameMap: Map[String, String]

  override def emitValDef(s: Sym, rhs: =>String): Unit = {
    if (dce.live(s))
      super.emitValDef(s,rhs)
    else
      emit(rhs)
  }

  override def shallow(n: Def): String = n match {
    case InlineSym(t: Node) => shallow(t)
    case b:Block => quoteBlock1(b)
    case _ => quote(n)
  }

  private val headers = mutable.HashSet[String]("<stdio.h>", "<stdlib.h>", "<stdint.h>","<stdbool.h>")
  def registerHeader(nHeaders: String*) = headers ++= nHeaders.toSet
  def emitHeaders(out: PrintStream) = headers.foreach { f => out.println(s"#include $f") }

  private val registeredFunctions = mutable.HashSet[String]()
  private val functionsStream = new ByteArrayOutputStream()
  private val functionsWriter = new PrintStream(functionsStream)
  def registerTopLevelFunction(id: String)(f: => Unit) = if (!registeredFunctions(id)) {
    // utils.withOutput(functionsWriter)(f)
    registeredFunctions += id
    captureLines(f).foreach(functionsWriter.println)
  }
  def emitFunctions(out: PrintStream) = if (functionsStream.size > 0){
    out.println("\n/************* Functions **************/")
    functionsStream.writeTo(out)
  }

  private val registeredDatastructures = mutable.HashSet[String]()
  private val datastructuresStream = new ByteArrayOutputStream()
  private val datastructuresWriter = new PrintStream(datastructuresStream)
  def registerDatastructures(id: String)(f: => Unit) = if (!registeredDatastructures(id)) {
    registeredDatastructures += id
    utils.withOutput(datastructuresWriter)(f)
  }
  def emitDatastructures(out: PrintStream) = if (datastructuresStream.size > 0) {
    out.println("\n/*********** Datastructures ***********/")
    datastructuresStream.writeTo(out)
  }

  val binop = Set("+","-","*","/","%","==","!=","<",">",">=","<=","&","|","<<",">>", "Boolean.&&", "Boolean.||")
  override def shallow(n: Node): String = n match {
    case n @ Node(s,op,args,_) if nameMap contains op =>
      shallow(n.copy(op = nameMap(n.op)))
    case n @ Node(f, "λforward",List(y),_) => ??? // this case is short cut at traverse function!
    case n @ Node(s, op,List(x,y),_) if binop(op) =>
      s"${shallow1(x, precedence(op))} $op ${shallow1(y, precedence(op)+1)}"
    case n @ Node(s,"Boolean.!",List(a),_) =>
      s"!${shallow1(a)}"
    case n @ Node(s,"Boolean.&&",List(a,b),_) =>
      s"${shallow1(a)} && ${shallow1(b)}"
    case n @ Node(s,"Boolean.||",List(a,b),_) =>
      s"${shallow1(a)} || ${shallow1(b)}"
    case n @ Node(s,op,args,_) if op.startsWith("unchecked") => // unchecked
      var next = 9 // skip unchecked
      var s = ""
      for (a <- args) {
        val i = op.indexOf("[ ]", next)
        assert(i >= next)
        s += op.substring(next,i)
        s += shallow(a)
        next = i + 3
      }
      s + op.substring(next)
    case n @ Node(s,op,args,_) if op.startsWith("String") => // String methods
      registerHeader("<string.h>")
      (op.substring(7), args) match {
        case ("equalsTo", List(lhs, rhs)) => s"(strcmp(${shallow(lhs)}, ${shallow(rhs)}) == 0)"
        case (a, _) => System.out.println(s"TODO: $a - ${args.length}"); ???
      }
    case n @ Node(s,op,args,_) if op.contains('.') && !op.contains(' ') => // method call
      val (recv::args1) = args
      if (args1.length > 0)
        s"${shallow1(recv)}.${op.drop(op.lastIndexOf('.')+1)}(${args1.map(shallow).mkString(",")})"
      else
        s"${shallow1(recv)}.${op.drop(op.lastIndexOf('.')+1)}"
    case n @ Node(s,"?",List(c,a,b:Block),_) if b.isPure && b.res == Const(false) =>
      s"${shallow(c)} && ${shallow(a)}"
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
  override def apply(g: Graph): Unit = {
    utils.withOutput(stream) {
      val ls = captureLines {
        bound(g)
        withScope(Nil, g.nodes) {
          traverse(g.block)
        }
      }
      if (g.block.res == Const(())) {
        ls.foreach(println)
      } else {
        ls.init.foreach(println)
        println(s"return ${ls.last};")
      }
    }
  }
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
  def record(man: RefinedManifest[_]): String = {
    val tpe = "struct " + man.toString
    registerDatastructures(tpe) {
      print(tpe); println(" {")
      man.fields.foreach {
        case (name, man) => println(remap(man) + " " + name + ";")
      }
      println("};")
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

  // block of statements
  override def quoteBlock(f: => Unit) = {
    val ls = captureLines(f)
    if (ls.length != 1) {
      "{\n" + ls.mkString("\n") + "\n}"
    } else ls.mkString("\n")
  }
  // block of statements with result expression
  def quoteBlockP(f: => Unit) = {
    val ls = captureLines(f)
    if (ls.length != 1) {
      "({\n" + ls.mkString("\n") + ";\n})"
    } else ls.mkString("\n")
  }

  def emitFunction(name: String, body: Block) = {
    val res = body.res
    val args = body.in
    emit(s"${remap(typeBlockRes(res))} $name(${args map(s => s"${remap(typeMap.getOrElse(s, manifest[Unknown]))} ${quote(s)}") mkString(", ")}) {")
    if (res == Const(())) {
      traverse(body)
      emit("}")
    } else {
      val ls = captureLines(traverse(body))
      ls.init.foreach(emit)
      emit(s"return ${ls.last};\n}")
    }
  }

  override def emitValDef(s: Sym, rhs: =>String): Unit = {
    // emit(s"val ${quote(s)} = $rhs; // ${dce.reach(s)} ${dce.live(s)} ")
    if (dce.live(s))
      emit(s"${remap(typeMap.getOrElse(s, manifest[Unknown]))} ${quote(s)} = $rhs;")
    else
      emit(s"$rhs;")
  }
  def emitVarDef(s: Sym, rhs: =>String): Unit = {
    // emit(s"var ${quote(s)} = $rhs; // ${dce.reach(s)} ${dce.live(s)} ")
    // TODO: enable dce for vars ... but currently getting unused expression warnings ...
    // if (dce.live(s))
      emit(s"${remap(typeMap.getOrElse(s, manifest[Unknown]))} ${quote(s)} = $rhs;")
    // else
      // emit(s"$rhs;")
  }
  override def shallow(n: Node): String = n match {
    case Node(s, op, List(a), _) if op.endsWith("toInt") =>
      s"(int)${shallow1(a)}"
    case Node(s, op, List(a), _) if op.endsWith("toFloat") =>
      s"(float)${shallow1(a)}"
    case Node(s, op, List(a), _) if op.endsWith("toDouble") =>
      s"(double)${shallow1(a)}"
    case Node(s, op, List(a), _) if op.endsWith("toLong") =>
      s"(long)${shallow1(a)}"
    case Node(s,"String.charAt",List(a,i),_) =>
      s"${shallow1(a)}[${shallow(i)}]"
    case Node(s,"array_get",List(a,i),_) =>
      s"${shallow1(a)}[${shallow(i)}]"
    // case Node(s,"var_get",List(a),_) =>
      // quote(a)+s"/*${quote(s)}*/"

    // case n @ Node(s,"comment",_,_) =>
      // s"(${super.shallow(n)})" // GNU C block expr
    case n @ Node(s,"?",List(c,a:Block,b:Block),_) if b.isPure && b.res == Const(false) =>
      s"${shallow(c)} && ${quoteBlockP(traverse(a))}"
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      s"${shallow1(c)} ? " +
      quoteBlockP(traverse(a)) +
      s" : " +
      quoteBlockP(traverse(b)) + ""

    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      s"while (" +
      quoteBlockP(traverse(c)) +
      s") " +
      quoteBlock1(b)
    case n @ Node(s,"generate-comment",List(Const(x)),_) =>
      s"// $x"
    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      quoteBlockP {
        emit("//#" + str)
        if (verbose) {
          emit("// generated code for " + str.replace('_', ' '))
        } else {
          emit("// generated code")
        }
        traverse(b)
        emit(";//#" + str)
      }

    case n @ Node(s,"P",List(x),_) =>
      s"printf(${"\"%s\\n\""}, ${shallow(x)})"
    case n @ Node(s, "timestamp", _, _) =>
      registerHeader("<sys/time.h>")
      emit(s"struct timeval ${quote(s)}_t;")
      emit(s"gettimeofday(&${quote(s)}_t, NULL);")
      s"${quote(s)}_t.tv_sec * 1000000L + ${quote(s)}_t.tv_usec"
    case n @ Node(s, op, List(struct), _) if op.startsWith("read")=>
      s"${quote(struct)}->${op.substring(5)}"
    case n @ Node(s, "λtop", List(block: Block), _) =>
      registerTopLevelFunction(quote(s)) {
        emitFunction(quote(s), block)
      }
      quote(s)
    case n @ Node(s, "reffield_get", List(ptr, Const(field)), _) =>
      s"${quote(ptr)}->$field"
    case n @ Node(s, "ref_new", List(v), _) =>
      s"&${quote(v)}"
    case n @ Node(s, "NewArray" ,List(x), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      s"($tpe*)malloc(${shallow(x)} * sizeof($tpe))"
    case n @ Node(s,"array_new",List(x),_) =>
      s"(int*)malloc(${shallow1(x)} * sizeof(int))"
    case n @ Node(s,"array_sort",List(arr, len, arr2, comp),_) => // FIXME: not arr duplicated?
      s"qsort(${shallow1(arr)}, ${shallow1(len)}, sizeof(*${shallow(arr2)}), (__compar_fn_t)${shallow(comp)})" // shallow(comp) to trigger generation of function
    case n =>
      super.shallow(n)
  }
  override def traverse(n: Node): Unit = n match {
    // case n @ Node(s,"P",_,_) => // Unit result
    //   emit(shallow(n))
    // case n @ Node(s,"W",_,_) => // Unit result
    //   emit(shallow(n))
    case n @ Node(s,"var_new",List(x),_) =>
      emitVarDef(s, shallow(x))
    case n @ Node(s, "local_struct", Nil, _) =>
      emitValDef(s, "{ 0 }") // FIXME: uninitialized? or add it as argument?
    case n @ Node(s,"var_set",List(x,y),_) =>
      emit(s"${quote(x)} = ${shallow(y)};")
    case n @ Node(s, "reffield_set", List(ptr, Const(field), v), _) =>
      emit(s"${quote(ptr)}->$field = ${shallow(v)};")
    // static array
    case n @ Node(s, "Array" , xs,_) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"$tpe ${quote(s)}[${xs.length}] = { ${xs.map(shallow).mkString(", ")} };");

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
      emit(s"${shallow1(x)}[${shallow(i)}] = ${shallow(y)};")


    case n @ Node(s,"?",c::(a:Block)::(b:Block)::_,_) if !dce.live(s) =>
      emit(s"if (${shallow(c)}) " +
      quoteBlock(traverse(a.copy(res = Const(())))) +
      s" else " +
      quoteBlock(traverse(b.copy(res = Const(())))))

    case n @ Node(s,"W",List(c:Block,b:Block),_) if !dce.live(s) =>
      emit(s"while (" +
      quoteBlockP(traverse(c)) +
      s") " +
      quoteBlock(traverse(b)))

    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_)  if !dce.live(s) =>
        emit("//#" + str)
        if (verbose) {
          emit("// generated code for " + str.replace('_', ' '))
        } else {
          emit("// generated code")
        }
        traverse(b)
        emit(";//#" + str)
    case n @ Node(s, "λforward", List(y), _) =>
      forwardMap(s) = y // for this case, adding (f, y) in forwardMap is all we need
    case n @ Node(s, "λtop", List(block@Block(args, res, _, _)), _) => ??? // shouldn't be possible in C
    case n @ Node(s, op,_,_) =>
      // emit(s"val ${quote(s)} = " + shallow(n))
      emitValDef(s, shallow(n))
  }

  def run(name: String, g: Graph): List[String] = {
    captureLines {
      bound(g)
      withScope(Nil, g.nodes) {
        emitFunction(name, g.block)
      }
    }
  }

  def convert(arg: String, man: Manifest[_]): String = {
    if (man == manifest[Int]) s"atoi($arg)"
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
    src.foreach(stream.println)
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
