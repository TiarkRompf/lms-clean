package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream, File, PrintWriter}

import Backend._

class ExtendedCCodeGen extends CompactCodeGen with ExtendedCodeGen {
  var lastNL = false
  override def emit(s: String): Unit = { stream.print(s); lastNL = false }
  override def emitln(s: String = "") = if (s != "" || !lastNL) { stream.println(s); lastNL = true }

  override def quote(x: Def) = x match {
    case Const(scala.Int.MinValue) => "0x" + scala.Int.MinValue.toHexString
    case Const(scala.Long.MinValue) => "0x" + scala.Long.MinValue.toHexString + "L"
    case Const(x: Double) if x.isNaN => "NAN"
    case Const(x: Double) if x.isInfinity => "INFINITY"
    case Const(x: Float) if x.isNaN => "NAN"
    case Const(x: Float) if x.isInfinity => "INFINITY"
    case Const(null) => "NULL"
    case Const(c: Char) if c.toInt < 0x20 || c.toInt > 0x7F =>
      val res = super.quote(x) // if escaped
      if (res.charAt(1) == '\\') res else "0x" + c.toHexString
    case Const(x: List[_]) => "{" + x.mkString(", ") + "}" // translate a Scala List literal to C List literal
    case _ =>
      super.quote(x)
  }

  // Remap auxiliary function C specific
  def primitive(rawType: String): String = rawType match {
    case "Unit" => "void"
    case "Boolean" => "bool"
    case "java.lang.String" => "char*"
    // case "Nothing" | "Any" => ???
    case _ => rawType.toLowerCase
  }
  def remapUnsigned(m: Manifest[_]): String = "unsigned " + remap(m)
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
  override val nameMap = Map( // FIXME: tutorial specific
    "ScannerNew"     -> "new scala.lms.tutorial.Scanner",
    "ScannerHasNext" -> "Scanner.hasNext",
    "ScannerNext"    -> "Scanner.next",
    "ScannerClose"   -> "Scanner.close",
    "ObjHashCode"    -> "Object.hashCode",
    "StrSubHashCode" -> "hash"
  )

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
  final def precedence(op: String): Int = op match {
    case "?" => 1
    case "||" => 2
    case "&&" => 3
    case "|" => 4
    case "^" => 5
    case "&" => 6
    case "==" | "!=" | "String.equalsTo" => 7
    case "<" | ">" | "<=" | ">=" => 8
    case "<<" | ">>" | ">>>" => 9
    case "+" | "-" | "array_slice" | "String.slice" => 10
    case "*" | "/" | "%" => 11
    case "cast" => 12
    case "ref_new" => 13 // & address of
    case "reffield_get" => 14 // -> pointer member access
    // type casting is lower in precedence?
    case "NewArray" | "mmap" => 19 // there might be type conversion before array access, which should be put in parenthesis
    case _ if op.startsWith("unchecked") => 0 // force parenthesis if nested: 3 * unchecked(5 + 4)
    case _ => 20
  }

  def emitFunctionSignature(name: String, body: Block, decorator: String = "",
                            argNames: Boolean = true, ending: String = " ") = {
    val res = body.res
    val args = body.in
    val ret = if (decorator == "") s"${remap(typeBlockRes(res))} "
              else s"$decorator ${remap(typeBlockRes(res))} "
    val params = "(" + args.map { s =>
      val ty = remap(typeMap.getOrElse(s, manifest[Unknown]))
      val name = quote(s)
      if (argNames) ty + " " + name else ty
    }.mkString(", ") + ")"
    val sig = ret + name + params + ending
    emit(sig)
  }

  def emitFunction(name: String, body: Block, decorator: String = "") = {
    emitFunctionSignature(name, body, decorator)
    quoteBlockPReturn(traverse(body))
    emitln()
  }

  // block of statements without produced value -- effect only
  override def quoteBlock(f: => Unit): Unit = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      assert(y.res == Const(()), s"You should use quoteBlockP maybe? Result: $y")
      if (numStms > 1) emitln("{") else if (numStms == 0) emit("{")
      f
      if (numStms > 1) emit("}") else if (numStms == 0) emit("}")
    }
    withWraper(wraper _)(f)
  }
  def quoteBlock(b: Block): Unit =
    throw new RuntimeException("Generating nested functions or closures is not supported yet in C codegen")
  def quoteBlock(header: String)(f: => Unit): Unit =
    throw new RuntimeException("Generating nested functions or closures is not supported yet in C codegen")
  def quoteBlock(b: Block, argType: Boolean = false): Unit =
    throw new RuntimeException("Generating nested functions or closures is not supported yet in C codegen")

  // block of statements with result expression
  def quoteBlockPReturn(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      emitln("{")
      f
      if (y.res != Const(()))
        es"return ${y.res};"
      emitln(quoteEff(y.eff))
      emit("}")
    }
    withWraper(wraper _)(f)
  }

  // block of statements with produced value.
  /* Generate braces:
   *  - if there is no statement and no return value (empty block)  // only for quoteBlock?
   *  - if there is at least one statement and a return value       // only for quoteBlockP?
   *  - if there are more than one statement
   *
   * n > 1                       n > 1                      n > 1
   * || n == 0 && res == () ==>  || n == 0 && res == () ==> || n == 0 && res == ()
   * || n > 0 && res != ()       || n == 1 && res != ()     || n != 0 && res != ()
   *
   * ==>
   *
   * n > 1 && (n == 0 ^^ res != ())
   */
  override def quoteBlockP(f: => Unit) = quoteBlockP(0)(f)
  override def quoteBlockP(prec: Int)(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      assert(y.res != Const(()), "You should use quoteBlock maybe?")
      val paren = numStms > 0 || l.map(n => precedence(n) < prec).getOrElse(false)
      val brace = numStms > 0 // || (numStms == 0 ^ y.res != Const(()))
      if (paren) emit("(")
      if (brace) emitln("{")
      f
      shallow(y.res); emit(quoteEff(y.eff));
      // if (numStms > 0) emitln(";")
      if (brace) emit(";\n}")
      if (paren) emit(")")
    }
    withWraper(wraper _)(f)
  }
  // block of statement of an else branch
  override def quoteElseBlock(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      if (numStms > 0 /* y.res == Const(()) should always be true for else */) {
        assert(y.res == Const(()))
        emit(" else ")
        if (numStms > 1) emitln("{")
        f
        if (numStms > 1) emit("}")
      }
    }
    withWraper(wraper _)(f)
  }

  override def shallow(n: Def): Unit = n match {
    case InlineSym(t: Node) => shallow(t)
    case b:Block => quoteBlock(b)
    case _ => emit(quote(n))
  }

  protected val defines = mutable.HashSet[String]()
  def registerDefine(define: String*) = defines ++= define.toSet

  protected val headers = mutable.HashSet[String]("<stdio.h>", "<stdlib.h>", "<stdint.h>","<stdbool.h>")
  def registerHeader(nHeaders: String*): Unit = headers ++= nHeaders.toSet
  def registerHeader(includePath: String, header: String): Unit = {
    registerIncludePath(includePath)
    registerHeader(header)
  }
  def unregisterHeader(nHeaders: String*): Unit = headers --= nHeaders.toSet

  def cleanFlatDir(dir: java.nio.file.Path): Unit = {
    import java.nio.file._
    import java.nio.file.StandardCopyOption._
    Files.walk(dir).forEach {f => if (!Files.isDirectory(f)) Files.delete(f) }
    Files.deleteIfExists(dir)
  }

  def prepareHeaders: Unit = {
    import java.nio.file._
    import java.nio.file.StandardCopyOption._

    val classLoader = getClass.getClassLoader
    val src = Paths.get(classLoader.getResource("headers").getFile)
    val dst = Paths.get(System.getProperty("user.dir")).resolve("headers")
    var srcSum = 0; Files.walk(src).forEach {f => srcSum += 1};
    var dstSum = 0; if (Files.exists(dst)) Files.walk(dst).forEach {f => dstSum += 1};
    if (dstSum < srcSum && Files.exists(dst)) cleanFlatDir(dst)
    if (!Files.exists(dst)) {
      Files.walk(src).forEach { f =>
        Files.copy(f, dst.resolve(src.relativize(f)), REPLACE_EXISTING)
      }
    }

    registerIncludePath(dst.toString)
  }

  def emitHeaders(out: PrintStream) = headers.foreach { f =>
    out.println(s"#include $f")
  }

  def emitDefines(out: PrintStream) = defines.foreach { f =>
    out.println(s"#ifndef $f")
    out.println(s"#define $f")
    out.println(s"#endif")
  }

  val includePaths = mutable.HashSet[String]()
  def registerIncludePath(paths: String*) = includePaths ++= paths.toSet

  val libraryFlags = mutable.HashSet[String]()
  def registerLibrary(nLibraries: String*) = libraryFlags ++= nLibraries.toSet

  val libraryPaths = mutable.HashSet[String]()
  def registerLibraryPath(paths: String*) = libraryPaths ++= paths.toSet

  def joinPaths(paths: Iterable[String], option: String): String = {
    if (paths.isEmpty) ""
    else s"$option ${paths.mkString(s" $option ")}"
  }

  protected val registeredFunctions = mutable.HashSet[String]()
  protected val functionsStreams = new mutable.LinkedHashMap[String, (PrintStream, ByteArrayOutputStream)]()
  protected val ongoingFun = new mutable.HashSet[String]()
  def registerTopLevelFunction(id: String, streamId: String = "general")(f: => Unit) =
    if (!registeredFunctions(id)) {
      if (ongoingFun(streamId)) ???
      ongoingFun += streamId
      registeredFunctions += id
      withStream(functionsStreams.getOrElseUpdate(streamId, {
        val functionsStream = new ByteArrayOutputStream()
        val functionsWriter = new PrintStream(functionsStream)
        (functionsWriter, functionsStream)
      })._1)(f)
      ongoingFun -= streamId
    }
  protected val functionDeclsStreams = new mutable.LinkedHashMap[String, (PrintStream, ByteArrayOutputStream)]()
  def registerTopLevelFunctionDecl(id: String, streamId: String = "general")(f: => Unit) = {
    withStream(functionDeclsStreams.getOrElseUpdate(streamId, {
      val functionsStream = new ByteArrayOutputStream()
      val functionsWriter = new PrintStream(functionsStream)
      (functionsWriter, functionsStream)
    })._1)(f)
  }

  def emitFunctionDecls(out: PrintStream): Unit =
    if (functionDeclsStreams.size > 0){
      out.println("\n/************* Function Declarations **************/")
      for ((_, functionsStream) <- functionDeclsStreams.values.toSeq) {
        functionsStream.writeTo(out)
      }
    }

  def emitFunctions(out: PrintStream): Unit =
    if (functionsStreams.size > 0){
      out.println("\n/************* Functions **************/")
      for ((_, functionsStream) <- functionsStreams.values.toSeq.reverse) // ensure dependencies
        functionsStream.writeTo(out)
    }

  protected val registeredDatastructures = mutable.HashSet[String]()
  protected val datastructuresStream = new ByteArrayOutputStream()
  protected val datastructuresWriter = new PrintStream(datastructuresStream)
  protected var ongoingData = false
  def registerDatastructures(id: String)(f: => Unit): Unit =
    if (!registeredDatastructures(id)) {
      if (ongoingData) ???
      ongoingData = true
    registeredDatastructures += id
    withStream(datastructuresWriter)(f)
    ongoingData = false
    }
  def emitDatastructures(out: PrintStream): Unit =
    if (datastructuresStream.size > 0) {
      out.println("\n/*********** Datastructures ***********/")
      datastructuresStream.writeTo(out)
    }

  protected val registeredInit = mutable.HashSet[String]()
  protected val initStream = new ByteArrayOutputStream()
  protected val initWriter = new PrintStream(initStream)
  protected var ongoingInit = false
  def registerInit(id: String)(f: => Unit) = if (!registeredInit(id)) {
    if (ongoingInit) ???
    ongoingInit = true
    registeredInit += id
    withStream(initWriter)(f)
    ongoingInit = false
  }

  def emitInit(out: PrintStream) = if (initStream.size > 0) {
    out.println("\n/*********** Init ***********/")
    out.println("inline int init() {")
    initStream.writeTo(out)
    out.println("  return 0;\n}")
  }

  def emitValDef(n: Node): Unit = {
    if (dce.live(n.n))
      emit(s"${remap(typeMap.getOrElse(n.n, manifest[Unknown]))} ${quote(n.n)} = ")
    esln"$n;"
  }

  def emitVarDef(n: Node): Unit = {
    // emit(s"var ${quote(s)} = $rhs; // ${dce.reach(s)} ${dce.live(s)} ")
    // TODO: enable dce for vars ... but currently getting unused expression warnings ...
    // if (dce.live(s))
    esln"${remap(typeMap.getOrElse(n.n, manifest[Unknown]))} ${quote(n.n)} = ${n.rhs.head};"
    // else
    // emit(s"$rhs;")
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, ">>>", List(a, b), _) =>
      emit("(("); emit(remapUnsigned(typeMap.getOrElse(s, manifest[Unknown]))); emit(") ")
      shallowP(a, precedence("cast") + 1); emit(") >> "); shallowP(b, precedence(">>") + 1)
    case Node(s, op, List(a), _) if math.contains(op) =>
      registerHeader("<math.h>")
      registerLibrary("-lm")
      es"$op($a)"
    case Node(s, "cast", a::_, _) =>
      // NOTE: removing upcast can lead to errors because if inlining:
      // int32_t x = ...
      // int64_t hash = (x << 6) + ...
      // is different from
      // int32_t x = ...
      // int64_t hash = ((int64_t)x << 6) + ...
      val tpe = typeMap.getOrElse(s, manifest[Unknown])
      emit(s"(${remap(tpe)})"); shallowP(a, precedence("cast"))
    case Node(s,"String.charAt",List(a,i),_) =>
      shallowP(a); es"[$i]"
    case Node(s,"array_get",List(a,i),_) =>
      shallowP(a); es"[$i]"
    // case Node(s,"var_get",List(a),_) =>
      // quote(a)+s"/*${quote(s)}*/"

    // case n @ Node(s,"comment",_,_) =>
      // s"(${super.shallow(n)})" // GNU C block expr
     // Have to be duplicated to be bfore generic C version
    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if b.isPure && b.res == Const(false) =>
      shallowP(c, precedence("&&")); emit(" && "); quoteBlockP(precedence("&&") + 1)(traverse(a))
    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if a.isPure && a.res == Const(true) =>
      shallowP(c, precedence("||")); emit(" || "); quoteBlockP(precedence("||") + 1)(traverse(b))
    case n @ Node(f,"?",c::(a:Block)::(b:Block)::_,_) =>
      shallowP(c, precedence("?") + 1); emit(" ? ")
      quoteBlockP(precedence("?") + 1)(traverse(a))
      emit(" : ")
      quoteBlockP(precedence("?") + 1)(traverse(b))
    case n @ Node(f,"W",List(c:Block,b:Block),_) => ???
    case n @ Node(s,"generate-comment",List(Const(x: String)),_) => ???

    case n @ Node(s,"P",List(x),_) =>
      es"""printf("%s\n", $x)"""
    case n @ Node(s, "λ", (block: Block)::Const(arity:Int)::Nil, _) =>
      registerTopLevelFunctionDecl(quote(s)) {
        emitFunctionSignature(quote(s), block, argNames = false, ending = ";\n")
      }
      registerTopLevelFunction(quote(s)) {
        emitFunction(quote(s), block)
      }
      emit(quote(s))
    case n @ Node(s, "λ", (block: Block)::Const(arity:Int)::Const(decorator: String)::Nil, _) =>
      registerTopLevelFunctionDecl(quote(s)) {
        emitFunctionSignature(quote(s), block, argNames = false, ending = ";\n")
      }
      registerTopLevelFunction(quote(s)) {
        emitFunction(quote(s), block, decorator)
      }
      emit(quote(s))
    case n @ Node(s, "reffield_get", List(ptr, Const(field: String)), _) =>
      shallowP(ptr, precedence("reffield_get")); emit("->"); emit(field)
    case n @ Node(s, "ref_new", List(v), _) =>
      emit("&"); shallowP(v, precedence("ref_new"))
    case n @ Node(s, "NewArray" ,List(x), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"($tpe*)malloc("); shallowP(x, precedence("*")); emit(s" * sizeof($tpe))")
    case n @ Node(s, "mmap" ,List(len, fd), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      es"($tpe*)mmap(0, $len, PROT_READ, MAP_FILE | MAP_SHARED, $fd, 0)"
    case n @ Node(s, "NewArray" ,List(x, Const(0)), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      es"($tpe*)calloc($x, sizeof($tpe))"
    case n @ Node(s,"array_new",List(x),_) =>
      emit(s"(int*)malloc("); shallowP(x, precedence("*")); emit(s" * sizeof(int))")
    case n @ Node(s,"array_slice",List(x, idx, _), _) =>
      shallowP(x, precedence("+")); emit(" + "); shallowP(idx, precedence("+") + 1)
    case n @ Node(s,"array_length",List(x), _) => ???
    case n @ Node(s,"array_sort",List(arr, len, arr2, comp),_) => // FIXME: not arr duplicated?
      es"qsort($arr, $len, sizeof(*$arr2), (__compar_fn_t)$comp)"
    case n @ Node(s,"array_free", List(arr), _) =>
      es"free($arr)"
    case n @ Node(s,"array_resize", List(arr, nsize), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      es"($tpe*)realloc($arr, "; shallowP(nsize, precedence("*")); emit(s" * sizeof($tpe))")
    case n @ Node(s, "array-of-char-to-string", List(x:Sym), _) =>
      shallow(x) // FIXME(feiw): this is currently wrong: should have array copy
    case n @ Node(s,op,args,_) if nameMap contains op =>
      shallow(n.copy(op = nameMap(n.op)))
    case n @ Node(f, "λforward", _, _) => ??? // this case is short cut at traverse function!
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
        case ("slice", List(lhs, idx, _)) => shallowP(lhs, precedence("+")); emit(" + "); shallowP(idx, precedence("+") + 1)
        case ("equalsTo", List(lhs, rhs)) => es"strcmp($lhs, $rhs) == 0"
        case ("toDouble", List(rhs)) => es"atof($rhs)"
        case ("toInt", List(rhs)) => es"atoi($rhs)"
        case ("length", List(rhs)) => es"strlen($rhs)"
        case (a, _) => System.out.println(s"TODO: $a - ${args.length}"); ???
      }
    case n @ Node(s,op,args,_) if op.contains('.') && !op.contains(' ') => // method call
      val (recv::args1) = args
      if (args1.length > 0) {
        shallowP(recv)
        es".${op.drop(op.lastIndexOf('.') + 1)}(${args1.head}"
        args1.tail.foreach { x => es", $x" }
        emit(")")
      } else {
        shallowP(recv); emit("."); emit(op.drop(op.lastIndexOf('.') + 1))
      }
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
      esln"${quote(x)} = $y;"
    case n @ Node(s, "reffield_set", List(ptr, Const(field: String), v), _) =>
      shallowP(ptr, precedence("reffield_get"))
      esln"->$field = $v;"
    // static array
    case n @ Node(s, "Array" , xs,_) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      es"$tpe ${quote(s)}[${xs.length}] = { ${xs.head}"
      xs.tail.foreach(x => es", $x")
      emitln(" };")

    case n @ Node(s, "NewArray", List(x, Const(v)), _) if v != 0 =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"long ${quote(s)}_len = "); shallowP(x, precedence("*")); emitln(s" * sizeof($tpe);")
      emitln(s"$tpe* ${quote(s)} = ($tpe*)malloc(${quote(s)}_len)")
      emitln(s"memset(${quote(s)}, $v, ${quote(s)}_len);")
    // DCE: FIXME:
    // (a) check that args have no side-effects
    //      maybe this can be ensured by overriding InlineSym?
    // DCE: FIXME:
    // (a) check that args have no side-effects
    //      maybe this can be ensured by overriding InlineSym?
    //      should never inline a live expr into something that's not live ...
    // (b) dce above should anticipate that these will be eliminated
    //      do not register dependencies as live!
    case n @ Node(s,"var_get",_,_) if !dce.live(s) => ??? // no-op
    case n @ Node(s,"array_get",_,_) if !dce.live(s) => ??? // no-op

    case n @ Node(s,"array_set",List(x,i,y),_) =>
      shallowP(x); esln"[$i] = $y;"

    case n @ Node(s, "array_copyTo", List(src, dst, start, len), _) =>
      emit("memcpy("); shallowP(dst, precedence("+")); emit(" + "); shallowP(start, precedence("+") + 1)
      esln", $src, $len);"

    case n @ Node(s,"?",c::(a:Block)::(b:Block)::_,_) if !dce.live(s) =>
      es"if ($c) "
      quoteBlock(traverse(a))
      quoteElseBlock(traverse(b))
      emitln()

    case n @ Node(s,"W",List(c:Block,b:Block),_) =>
      emit("while (")
      quoteBlockP(traverse(c))
      emit(") ")
      quoteBlock(traverse(b))
      emitln()

    case n @ Node(_, "switch", (guard: Exp)::default::others, _) =>
      val tp = typeBlockRes(guard)
      assert(tp == manifest[Long] || tp == manifest[Int] || tp == manifest[Short] || tp == manifest[Char])
      esln"switch ($guard) {"
      others.grouped(2).foreach {
        case Seq(Const(cases: Seq[Const]), block: Block) =>
          cases.foreach(x => emitln(s"case ${quote(x)}:;")) // extra ; used to avoid error on case 2: int x = 1; ....
          noquoteBlock(traverse(block))
          emitln("break;")
      }
      default match {
        case block: Block =>
          emitln("default:;")
          noquoteBlock(traverse(block))
          emitln("break;")
        case _ =>
      }
      emitln("}")

    case n @ Node(s,"generate-comment",List(Const(x: String)),_) =>
      emit("// "); emitln(x)

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
    case n @ Node(s, "λforward", List(y, arity), _) =>
      emitln("//# lambda forward is here!")
      // for this case, adding (f, y) in forwardMap is all we need
      // XXX: this is most likely not enough in the general case
      rename(s) = quote(y)
    case n @ Node(s, "λ", (block: Block)::Const(arity:Int)::Nil, _) => // FIXME: check free var?
      registerTopLevelFunctionDecl(quote(s)) {
        emitFunctionSignature(quote(s), block, argNames = false, ending = ";\n")
      }
      registerTopLevelFunction(quote(s)) {
        emitFunction(quote(s), block)
      }
    case n @ Node(s, "λ", (block: Block)::Const(arity:Int)::Const(decorator: String)::Nil, _) =>
      registerTopLevelFunctionDecl(quote(s)) {
        emitFunctionSignature(quote(s), block, argNames = false, ending = ";\n")
      }
      registerTopLevelFunction(quote(s)) {
        emitFunction(quote(s), block, decorator)
      }
    case n @ Node(s, "timestamp", _, _) =>
      registerHeader("<sys/time.h>")
      emitln(s"struct timeval ${quote(s)}_t;")
      emitln(s"gettimeofday(&${quote(s)}_t, NULL);")
      emitln(s"long ${quote(s)} = ${quote(s)}_t.tv_sec * 1000000L + ${quote(s)}_t.tv_usec;");
    case n @ Node(s, "exit", List(x) ,_) =>
      esln"fflush(stdout); fflush(stderr); exit($x);"
    case _ =>
      emitValDef(n)
  }

  // FIXME Too ugly!!!!
  var graphCache: Map[Sym,Node] = _
  def isWritableSym(w: Sym): Boolean = graphCache.get(w) match {
    case Some(Node(_, "var_new", _, _)) => true // ok
    case _ => false
  }

  def run(name: String, g: Graph) = {
    capture {
      graphCache = g.globalDefsCache
      bound(g)
      withScope(Nil, g.nodes) {
        emitFunction(name, g.block)
      }
      graphCache = null
    }
  }

  def convert(arg: String, man: Manifest[_]): String = {
    if (man == manifest[Int]) s"atoi($arg)"
    else if (man == manifest[String]) arg
    else "" // manifest[Unit]
  }

  def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    val ng = init(g)
    val efs = "" //quoteEff(g.block.ein)
    val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
    prepareHeaders
    emitln("""
    |/*****************************************
    |Emitting C Generated Code
    |*******************************************/
    """.stripMargin)
    val src = run(name, ng)
    emitDefines(stream)
    emitHeaders(stream)
    emitDatastructures(stream)
    emitFunctions(stream)
    emitInit(stream)
    emitln(s"\n/**************** $name ****************/")
    emit(src)
    emitln("""
    |/*****************************************
    |End of C Generated Code
    |*******************************************/
    |int main(int argc, char *argv[]) {
    |  if (argc != 2) {
    |    printf("usage: %s <arg>\n", argv[0]);
    |    return 0;
    |  }""".stripMargin)
    if (initStream.size > 0) emitln("if (init()) return 0;")
    emitln(s"  $name(${convert("argv[1]", m1)});\n  return 0;\n}")
  }
}
