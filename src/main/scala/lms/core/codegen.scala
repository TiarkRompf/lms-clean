package lms.core

import scala.collection.mutable

import Backend._


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

