package lms.core

import scala.collection.mutable

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
    case n @ Node(s,"var_get",List(x),_) => 
      s"${shallow(x)}"
    case n @ Node(s,"array_get",List(x,i),_) => 
      s"${shallow(x)}(${shallow(i)})"
    case n @ Node(s,"@",x::y::_,_) => 
      s"${shallow(x)}(${shallow(y)})"
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

  def valueSyms(n: Node): List[Sym] = 
    directSyms(n) ++ blocks(n).collect { case Block(_,res:Sym,_,_) => res }

  var live: collection.Set[Sym] = _
  var reach: collection.Set[Sym] = _

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
        reach ++= syms(d)
        if (d.op == "staticData")
          statics += d
      }
    }

    //Graph(g.nodes.filter(d => live(d.n)), g.block)
  }
}

trait ExtendedCodeGen0 extends CompactTraverser {

  var typeMap: collection.Map[lms.core.Backend.Exp, Manifest[_]] = _

  val dce = new DeadCodeElimCG

  var stream: java.io.PrintStream = _

  def quote(s: Def): String
  def remap(m: Manifest[_]): String

  def init(g: Graph): Unit = { dce(g) }
}


class ExtendedScalaCodeGen extends ExtendedCodeGen0 {

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
  val nameMap: Map[String, String] = Map(
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
        emit("var "); emit(quote(d.n)); emit(": "); emit(remap(typeMap(d.n))); emitln(" = null")
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


  val binop = Set("+","-","*","/","%","==","!=","<",">","&","!")

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
    case n @ Node(s,"@",x::y::_,_) => 
      shallow1(x); emit("("); shallow(y); emit(")")
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

    case n @ Node(s,op,args,_) if op contains "[ ]" => // unchecked
      var s = op
      for (a <- args) {
        val i = s.indexOf("[ ]")
        emit(s.substring(0,i))
        shallow(a)
        s = s.substring(i+3)
      }
      emit(s)

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
    case n @ Node(f,"λ",List(y:Block),_) => 
      val x = y.in.head
      val a = typeMap(x)
      val b = typeMap(y.res)
      val e = quoteEff(y.ein)

      emit(s"def ${quote(f)}(${quote(x)}: $a): $b$e = ")
      quoteBlockp(traverse(y,f))
      emitln()

    case n @ Node(s,"generate-comment",List(Const(x)),_) => 
      emit("// "); emitln(x.toString)

    case n @ Node(s,"var_new",List(x),_) => 
      /*if (dce.live(s))*/ emit(s"var ${quote(s)} = "); shallow(x); emitln()
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

}

abstract class ExtendedCodeGen extends CompactScalaCodeGen with ExtendedCodeGen0 {

  // var typeMap: collection.Map[lms.core.Backend.Exp, Manifest[_]] = _

  // val dce = new DeadCodeElim
  doRename = true

  override def quote(x: Def) = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n","\\n").replace("\t","\\t")+"\"" // TODO: more escapes?
    case Const(x) if x.isInstanceOf[Char] && x == '\n' => "'\\n'"
    case Const(x) if x.isInstanceOf[Char] && x == '\t' => "'\\t'"
    case Const(x) if x.isInstanceOf[Char] && x == 0    => "'\\0'"
    case Const(x) if x.isInstanceOf[Long] => x.toString + "L"
    case _ => super.quote(x)
  }

  // XXX proper operator precedence
  def shallow1(n: Def): String = n match {
    case InlineSym(n) if n.op != "var_get" => s"(${shallow(n)})"
    case _ => shallow(n)
  }

  def remap(m: Manifest[_]): String
  def nameMap: Map[String, String]

  override def emitValDef(s: Sym, rhs: =>String): Unit = {
    if (dce.live(s))
      super.emitValDef(s,rhs)
    else
      emit(rhs)
  }

  override def shallow(n: Node): String = n match {
    case n @ Node(s,op,args,_) if nameMap contains op => 
      shallow(n.copy(op = nameMap(n.op)))
    case n @ Node(s,"<",List(a,b),_) => 
      s"${shallow(a)} < ${shallow(b)}"
    case n @ Node(s,">",List(a,b),_) => 
      s"${shallow(a)} > ${shallow(b)}"
    case n @ Node(s,"&",List(a,b),_) => 
      s"${shallow1(a)} & ${shallow1(b)}"
    case n @ Node(s,"Boolean.!",List(a),_) => 
      s"!${shallow1(a)}"
    case n @ Node(s,op,args,_) if op contains "[ ]" => // unchecked
      var s = op
      for (a <- args)
        s = s.replaceFirst("\\[ \\]",shallow(a))
      s
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
  override def apply(g: Graph): Unit = {
    utils.withOutput(stream)(super.apply(g))
  }
}

class Unknown // HACK: Sentinel for typeMap

class ExtendedCCodeGen extends ExtendedCodeGen {
  def remap(m: Manifest[_]): String = m.toString match {
    case "Unit" => "void"
    case "Boolean" => "bool"
    case "Char" => "char"
    case "Int" => "int"
    case "Long" => "long"
    case "java.lang.String" => "char*"
    case "Array[Char]" => "char*"
    case "Array[Int]" => "int*"
    case "Array[java.lang.String]" => "char**"
    case s => s
  }
  val nameMap = Map( // FIXME: tutorial specific
    "ScannerNew"     -> "new scala.lms.tutorial.Scanner",
    "ScannerHasNext" -> "Scanner.hasNext",
    "ScannerNext"    -> "Scanner.next",
    "ScannerClose"   -> "Scanner.close",
    "ObjHashCode"    -> "Object.hashCode",
    "StrSubHashCode" -> "hash"
  )
  /*override def quoteBlock1(y: Block, argType: Boolean = false) = {
    val res = super.quoteBlock1(y, argType)
    if (res contains "\n") {
      assert(res.take(2) == "{\n" && res.takeRight(2) == "\n}")
      val res1 = res.drop(2).dropRight(2)
      s"({\n$res1;\n})" // GNU C understands ({ ... ; }), (note the semicolon); TCC will need more unravelling ...
    } else if (res == "()" ){
      "({})"
    } else res
  }*/
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
  // override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
  //   // FIXME: ugly repetition
  //   for (n <- ns) {
  //     if (shouldInline(n.n).isEmpty)
  //       traverse(n)
  //   }
  //   if (y.res != Const(()))
  //     emit(shallow(y.res) + ";" + quoteEff(y.eff))
  //   else
  //     emit(quoteEff(y.eff))
  // }
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
    case Node(s,"Char.toInt",List(a),_) =>
      s"(int)${shallow1(a)}"
    case Node(s,"Long.toInt",List(a),_) =>
      s"(int)${shallow1(a)}"
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
    case n @ Node(s,"var_set",List(x,y),_) => 
      emit(s"${quote(x)} = ${shallow(y)};")

    case n @ Node(s,"array_new",List(x),_) => 
      emitValDef(s, s"(int*)malloc(${shallow1(x)} * sizeof(int));")
    case n @ Node(s,"new Array[Int]",List(x),_) => 
      emitValDef(s, s"(int*)malloc(${shallow1(x)} * sizeof(int));")
    case n @ Node(s,"new Array[Char]",List(x),_) => 
      emitValDef(s, s"(char*)malloc(${shallow1(x)} * sizeof(char));")
    case n @ Node(s,"new Array[java.lang.String]",List(x),_) => 
      emitValDef(s, s"(char**)malloc(${shallow1(x)} * sizeof(char*));")

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
      quoteBlock(traverse(a)) +
      s" else " +
      quoteBlock(traverse(b)) + "")

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

    case n @ Node(s,_,_,_) => 
      // emit(s"val ${quote(s)} = " + shallow(n)) 
      emitValDef(s, shallow(n))
  }
}
