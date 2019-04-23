package lms.core.stub

import lms.util.OverloadHack
import lms.macros.SourceContext
import lms.macros.EmbeddedControls

import scala.collection.{mutable, immutable}

import lms.core._
import lms.util._
import Backend._

import utils.time

object Global {
  val sc = new lms.util.ScalaCompile {}
}


object Adapter extends FrontEnd {

  // def emitScala(name: String)(m1:Manifest[_],m2:Manifest[_])(prog: Exp => Exp) = {
  //   emitCommon(name, new ExtendedScalaCodeGen)(m1, m2)(prog)
  // }

  // def emitC(name: String)(m1:Manifest[_],m2:Manifest[_])(prog: Exp => Exp) = {
  //   emitCommon(name, new ExtendedCCodeGen)(m1, m2)(prog)
  // }

  var typeMap: mutable.Map[lms.core.Backend.Exp, Manifest[_]] = _
  var funTable: List[(Backend.Exp, Any)] = _

  def emitCommon1(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream, verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)(m1:Manifest[_],m2:Manifest[_])(prog: Exp => Exp) =
    emitCommon(name, cg, stream, verbose, alt, eff)(m1, m2)(g.reify(prog))
  def emitCommon2(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream, verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)(m1:Manifest[_],m2:Manifest[_])(prog: (Exp, Exp) => Exp) =
    emitCommon(name, cg, stream, verbose, alt, eff)(m1, m2)(g.reify(prog))

  def emitCommon(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream, verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)(m1:Manifest[_],m2:Manifest[_])(prog: => Block) = {
    typeMap = new scala.collection.mutable.HashMap[lms.core.Backend.Exp, Manifest[_]]()
    funTable = Nil

    var g = time("staging") { program(prog) }

    def extra() =  if (verbose) utils.captureOut {
      println("// Raw:")
      g.nodes.foreach(println)

      println("// Generic Codegen:")
      (new CodeGen)(g)

      println("// Scala Codegen:")
      (new ScalaCodeGen)(g)

      println("// Compact Scala Codegen:")
      (new ExtendedScalaCodeGen)(g)
    } else ""

    time("codegen") {
      // val btstrm = new java.io.ByteArrayOutputStream((4 << 10) << 10) // 4MB
      // val stream = new java.io.PrintStream(btstrm)

      cg.typeMap = typeMap
      cg.stream = stream

      cg.emitAll(g,name)(m1,m2)

      // (btstrm.toString, cg.extractAllStatics)
      cg.extractAllStatics
    }
  }

  override def mkGraphBuilder() = new MyGraphBuilder()

  class MyGraphBuilder extends GraphBuilder {

    // def num[T](m: Manifest[T]): Numeric[T] = m match {
    //   case t: Manifest[Int] => implicitly[Numeric[Int]]
    //   case t: Manifest[Float] => implicitly[Numeric[Float]]
    //   case _ => ???
    // }

    // def wrap[T:Manifest](x: Any): T = x.asInstanceOf[T]

    // def isNum(x: Any) = x.isInstanceOf[Int] || x.isInstanceOf[Float]

    override def rewrite(s: String, as: List[Def]): Option[Exp] = (s,as) match {
      // staticData(as)(i) => staticData(as(i))
      case ("array_get", List(Def("staticData", List(Const(as: Array[_]))), Const(i:Int))) =>
        as(i) match {
          case a: Int => Some(Const(a))
          case a => Some(reflect("staticData", Const(a)))
        }

      // as(i) = as(i) => ()   side condition: no write in-between!
      case ("array_set", List(as:Exp, i, rs @ Def("array_get", List(as1, i1))))
        if as == as1 && i == i1 && curEffects.get(as) == Some(rs) =>
        Some(Const(()))

      case ("Array.length", List(Def("NewArray", Const(n)::_))) =>
        Some(Const(n))
      case ("Array.length", List(Def("Array", List(Const(as: Array[_]))))) =>
        Some(Const(as.length))

      case ("Boolean.!", List(Const(b: Boolean))) => Some(Const(!b))
      case ("==", List(Const(a), Const(b))) => Some(Const(a == b))
      case ("!=", List(Const(a), Const(b))) => Some(Const(a != b))
      case ("<=", List(Const(a: Int), Const(b: Int))) => Some(Const(a <= b))
      case ("<=", List(Const(a: Float), Const(b: Float))) => Some(Const(a <= b))
      case ("<=", List(Const(a: Long), Const(b: Long))) => Some(Const(a <= b))
      case ("<=", List(Const(a: Double), Const(b: Double))) => Some(Const(a <= b))
      // idea 1:
      // case ("<=", List(Const(a), Const(b))) if isNum(a) => Some(Const(a.asInstanceOf[Double] <= b.asInstanceOf[Double]))
      // idea 2:
      //   implicit val m: Manifest[Int] = typeMap(Const(a)).asInstanceOf[Manifest[Int]]
      //   val tmp = num[Int](m).lteq(wrap[Int](a), wrap[Int](b))
      //   Some(Const(tmp))
      // }
      case (">=", List(Const(a: Int), Const(b: Int))) => Some(Const(a >= b))
      case (">=", List(Const(a: Long), Const(b: Long))) => Some(Const(a >= b))
      case (">=", List(Const(a: Float), Const(b: Float))) => Some(Const(a >= b))
      case (">=", List(Const(a: Double), Const(b: Double))) => Some(Const(a >= b))
      case ("<", List(Const(a: Int), Const(b: Int))) => Some(Const(a < b))
      case ("<", List(Const(a: Long), Const(b: Long))) => Some(Const(a < b))
      case ("<", List(Const(a: Float), Const(b: Float))) => Some(Const(a < b))
      case ("<", List(Const(a: Double), Const(b: Double))) => Some(Const(a < b))
      case (">", List(Const(a: Int), Const(b: Int))) => Some(Const(a > b))
      case (">", List(Const(a: Long), Const(b: Long))) => Some(Const(a > b))
      case (">", List(Const(a: Float), Const(b: Float))) => Some(Const(a > b))
      case (">", List(Const(a: Double), Const(b: Double))) => Some(Const(a > b))

      case _  =>
        super.rewrite(s,as)
    }


    override def reflect(s: String, as: Def*): Exp = (s,as.toList) match {
      case ("+", List(Const(a:Int),Const(b:Int))) => Const(a+b)
      case ("+", List(Const(0),b:Exp)) => b
      case ("+", List(a:Exp,Const(0))) => a
      case ("-", List(Const(a:Int),Const(b:Int))) => Const(a-b)
      // case ("-", List(Const(0),b)) => b
      case ("-", List(a:Exp,Const(0))) => a
      case ("*", List(Const(a:Int),Const(b:Int))) => Const(a*b)
      case ("*", List(Const(1),b:Exp)) => b
      case ("*", List(a:Exp,Const(1))) => a
      case ("*", List(Const(0),b:Exp)) => Const(0)
      case ("*", List(a:Exp,Const(0))) => Const(0)
      case ("/", List(Const(a:Int),Const(b:Int))) => Const(a/b)
      case ("%", List(Const(a:Int),Const(b:Int))) => Const(a%b)

      // TBD: can't just rewrite, need to reflect block!
      // case ("?", List(Const(true),a:Block,b:Block)) => a

      // for now we implement the front-end method as a
      // a smart constructor (might revisit later)

      case p =>
        super.reflect(s, as:_*)
    }
  }

}




// DSLAPI CODE

//@virtualize
trait UtilOps extends Base { this: Dsl =>
  type Typ[T] = Manifest[T]
  def typ[T:Typ] = manifest[T]
  def manifestTyp[T:Typ] = manifest[T]
  implicit class HashCls[T: Typ](o: Rep[T]) {
    def HashCode(implicit pos: SourceContext):Rep[Long] = infix_HashCode(o)
    def HashCode(len: Rep[Int])(implicit pos: SourceContext):Rep[Long] = o match {
      case s:Rep[String] => infix_HashCodeS(s, len)
      //case _ => infix_HashCode(o) //FIXME is this an ok dispatch?
    }
  }
  def infix_HashCode[T:Typ](a: Rep[T])(implicit pos: SourceContext): Rep[Long]
  def infix_HashCodeS(s: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext): Rep[Long]
}

//@virtualize
trait UtilOpsExp extends UtilOps with BaseExp { this: DslExp =>

  case class ObjHashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext) extends Def[Long] { def m = typ[T] }
  case class StrSubHashCode(o: Rep[String], len: Rep[Int])(implicit pos: SourceContext) extends Def[Long]
  def infix_HashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext) = ObjHashCode(o)
  def infix_HashCodeS(o: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext) = StrSubHashCode(o,len)

  //STUB
  // override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
  //   case e@ObjHashCode(a) => infix_HashCode(f(a))(e.m, pos)
  //   case e@StrSubHashCode(o,len) => infix_HashCodeS(f(o),f(len))
  //   case _ => super.mirror(e,f)
  // }).asInstanceOf[Exp[A]]
}



// @virtualize
trait Dsl extends PrimitiveOps with NumericOps with BooleanOps with LiftString with LiftPrimitives with LiftNumeric with LiftBoolean with IfThenElse with Equal with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps with SeqOps with Functions with While with StaticData with Variables with LiftVariables with ObjectOps with UtilOps {
  implicit def repStrToSeqOps(a: Rep[String]) = new SeqOpsCls(a.asInstanceOf[Rep[Seq[Char]]])
  // implicit class BooleanOps2(lhs: Rep[Boolean]) {
  //   def &&(rhs: =>Rep[Boolean])(implicit pos: SourceContext) =
  //   __ifThenElse(lhs, rhs, unit(false)) }
//  override def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean] = __ifThenElse(lhs, rhs, unit(false))
  def generate_comment(l: String): Rep[Unit]
  def comment[A:Typ](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A]
}

// @virtualize
trait DslExp extends Dsl with PrimitiveOpsExpOpt with NumericOpsExpOpt with BooleanOpsExp with IfThenElseExpOpt with EqualExpBridgeOpt with RangeOpsExp with OrderingOpsExp with MiscOpsExp with EffectExp with ArrayOpsExpOpt with StringOpsExp with SeqOpsExp with FunctionsRecursiveExp with WhileExp with StaticDataExp with VariablesExpOpt with ObjectOpsExpOpt with UtilOpsExp {
  //STUB
  // override def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = lhs match {
  //   case Const(false) => rhs
  //   case _ => super.boolean_or(lhs, rhs)
  // }
  // override def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = lhs match {
  //   case Const(true) => rhs
  //   case _ => super.boolean_and(lhs, rhs)
  // }

  // case class GenerateComment(l: String) extends Def[Unit]
  //def generate_comment(l: String) = ??? //STUB reflectEffect(GenerateComment(l))
  // case class Comment[A:Typ](l: String, verbose: Boolean, b: Block[A]) extends Def[A]
  //def comment[A:Typ](l: String, verbose: Boolean)(b: => Rep[A]): Rep[A] = ???

  //STUB
  // def comment[A:Typ](l: String, verbose: Boolean)(b: => Rep[A]): Rep[A] = {
  //   //b
  //   val br = reifyEffects(b)
  //   val be = summarizeEffects(br)
  //   super.reflectEffect[A](Comment(l, verbose, br), be)
  // }

  // override def boundSyms(e: Any): List[Sym[Any]] = e match {
  //   case Comment(_, _, b) => effectSyms(b)
  //   case _ => super.boundSyms(e)
  // }

  // override def array_apply[T:Typ](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = (x,n) match {
  //   case (Def(StaticData(x:Array[T])), Const(n)) =>
  //     val y = x(n)
  //     if (y.isInstanceOf[Int]) unit(y) else staticData(y)
  //   case _ => super.array_apply(x,n)
  // }

  // // TODO: should this be in LMS?
  // override def isPrimitiveType[T](m: Typ[T]) = (m == manifest[String]) || super.isPrimitiveType(m)
}

// @virtualize
trait DslGen extends ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenBooleanOps with ScalaGenIfThenElse
    with ScalaGenEqual with ScalaGenRangeOps with ScalaGenOrderingOps
    with ScalaGenMiscOps with ScalaGenArrayOps with ScalaGenStringOps
    with ScalaGenSeqOps with ScalaGenFunctions with ScalaGenWhile
    with ScalaGenStaticData with ScalaGenVariables
    with ScalaGenObjectOps
    with ScalaGenUtilOps {
  val IR: DslExp

  import IR._
/*STUB
  override def quote(x: Exp[Any]) = x match {
    case Const('\n') if x.tp == typ[Char] => "'\\n'"
    case Const('\t') if x.tp == typ[Char] => "'\\t'"
    case Const(0)    if x.tp == typ[Char] => "'\\0'"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Assign(Variable(a), b) =>
      emitAssignment(a.asInstanceOf[Sym[Variable[Any]]], quote(b))
    case IfThenElse(c,Block(Const(true)),Block(Const(false))) =>
      emitValDef(sym, quote(c))
    case PrintF(f:String,xs) =>
      emitValDef(sym, src"printf(${Const(f)::xs})")
    case GenerateComment(s) =>
      stream.println("// "+s)
    case Comment(s, verbose, b) =>
      stream.println("val " + quote(sym) + " = {")
      stream.println("//#" + s)
      if (verbose) {
        stream.println("// generated code for " + s.replace('_', ' '))
      } else {
        stream.println("// generated code")
      }
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("//#" + s)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
*/
}

// @virtualize
trait DslImpl extends DslExp { q =>
  val codegen = new DslGen {
    val IR: q.type = q
  }
}

// TODO: currently part of this is specific to the query tests. generalize? move?
// @virtualize
trait DslGenC extends CGenBase with CGenNumericOps
    with CGenPrimitiveOps with CGenBooleanOps with CGenIfThenElse
    with CGenEqual with CGenRangeOps with CGenOrderingOps
    with CGenMiscOps with CGenArrayOps with CGenStringOps
    with CGenSeqOps with CGenFunctions with CGenWhile
    with CGenStaticData with CGenVariables
    with CGenObjectOps
    with CGenUtilOps {
  val IR: DslExp
  import IR._

  //STUB
  // def getMemoryAllocString(count: String, memType: String): String = {
  //     "(" + memType + "*)malloc(" + count + " * sizeof(" + memType + "));"
  // }
  // override def remap[A](m: Typ[A]): String = m.toString match {
  //   case "java.lang.String" => "char*"
  //   case "Array[Char]" => "char*"
  //   case "Char" => "char"
  //   case _ => super.remap(m)
  // }
  // override def format(s: Exp[Any]): String = {
  //   remap(s.tp) match {
  //     case "uint16_t" => "%c"
  //     case "bool" | "int8_t" | "int16_t" | "int32_t" => "%d"
  //     case "int64_t" => "%ld"
  //     case "float" | "double" => "%f"
  //     case "string" => "%s"
  //     case "char*" => "%s"
  //     case "char" => "%c"
  //     case "void" => "%c"
  //     case _ =>
  //       import scala.virtualization.lms.internal.GenerationFailedException
  //       throw new GenerationFailedException("CGenMiscOps: cannot print type " + remap(s.tp))
  //   }
  // }
  // override def quoteRawString(s: Exp[Any]): String = {
  //   remap(s.tp) match {
  //     case "string" => quote(s) + ".c_str()"
  //     case _ => quote(s)
  //   }
  // }
  // // we treat string as a primitive type to prevent memory management on strings
  // // strings are always stack allocated and freed automatically at the scope exit
  // override def isPrimitiveType(tpe: String) : Boolean = {
  //   tpe match {
  //     case "char*" => true
  //     case "char" => true
  //     case _ => super.isPrimitiveType(tpe)
  //   }
  // }
  // // XX: from LMS 1.0
  // override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
  //   if (!isVoidType(sym.tp))
  //     stream.println(remapWithRef(sym.tp) + quote(sym) + " = " + rhs + ";")
  //   else // we might still want the RHS for its effects
  //     stream.println(rhs + ";")
  // }

/*STUB
  override def quote(x: Exp[Any]) = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"")+"\"" // TODO: more escapes?
    case Const('\n') if x.tp == typ[Char] => "'\\n'"
    case Const('\t') if x.tp == typ[Char] => "'\\t'"
    case Const(0)    if x.tp == typ[Char] => "'\\0'"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) =>
      val arrType = remap(a.m)
      stream.println(arrType + "* " + quote(sym) + " = " + getMemoryAllocString(quote(n), arrType))
    case ArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
    case ArrayUpdate(x,n,y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
    case PrintLn(s) => stream.println("printf(\"" + format(s) + "\\n\"," + quoteRawString(s) + ");")
    case StringCharAt(s,i) => emitValDef(sym, "%s[%s]".format(quote(s), quote(i)))
    case Comment(s, verbose, b) =>
      stream.println("//#" + s)
      if (verbose) {
        stream.println("// generated code for " + s.replace('_', ' '))
      } else {
        stream.println("// generated code")
      }
      emitBlock(b)
      emitValDef(sym, quote(getBlockResult(b)))
      stream.println("//#" + s)
    case _ => super.emitNode(sym,rhs)
  }
  override def emitSource[A:Typ](args: List[Sym[_]], body: Block[A], functionName: String, out: java.io.PrintWriter) = {
    withStream(out) {
      stream.println("""
      #include <fcntl.h>
      #include <errno.h>
      #include <err.h>
      #include <sys/mman.h>
      #include <sys/stat.h>
      #include <stdio.h>
      #include <stdint.h>
      #include <unistd.h>
      #ifndef MAP_FILE
      #define MAP_FILE MAP_SHARED
      #endif
      int fsize(int fd) {
        struct stat stat;
        int res = fstat(fd,&stat);
        return stat.st_size;
      }
      int printll(char* s) {
        while (*s != '\n' && *s != ',' && *s != '\t') {
          putchar(*s++);
        }
        return 0;
      }
      long hash(char *str0, int len)
      {
        unsigned char* str = (unsigned char*)str0;
        unsigned long hash = 5381;
        int c;

        while ((c = *str++) && len--)
          hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

        return hash;
      }
      void Snippet(char*);
      int main(int argc, char *argv[])
      {
        if (argc != 2) {
          printf("usage: query <filename>\n");
          return 0;
        }
        Snippet(argv[1]);
        return 0;
      }

      """)
    }
    super.emitSource[A](args, body, functionName, out)
  }
*/
}



// @virtualize
abstract class DslSnippet[A:Manifest, B:Manifest] extends Dsl {
  def wrapper(x: Rep[A]): Rep[B] = snippet(x)
  def snippet(x: Rep[A]): Rep[B]
}

// @virtualize
abstract class DslDriver[A:Manifest,B:Manifest] extends DslSnippet[A,B] with DslImpl with CompileScala {
  lazy val f = { val (c1,s1) = (code,statics); time("scalac") { Global.sc.compile[A,B]("Snippet", c1, s1) }}

  def precompile: Unit = f

  def precompileSilently: Unit = utils.devnull(f)

  def eval(x: A): B = { val f1 = f; time("eval")(f1(x)) }

  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource(wrapper, "Snippet", new java.io.PrintStream(source))(manifestTyp[A],manifestTyp[B])
    (source.toString, statics)
  }
}

// @virtualize
abstract class DslDriverC[A: Manifest, B: Manifest] extends DslSnippet[A, B] with DslExp {
  q =>
  val codegen = new DslGenC {
    val IR: q.type = q
  }
  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))
    (source.toString, statics)
  }
  lazy val f: A => Unit = {
    // TBD: should read result of type B?
    val out = new java.io.PrintStream("/tmp/snippet.c")
    out.println(code)
    out.close
    (new java.io.File("/tmp/snippet")).delete
    import scala.sys.process._
    time("gcc") { (s"cc -std=c99 -O3 /tmp/snippet.c -o /tmp/snippet": ProcessBuilder).lines.foreach(Console.println _) }
    (a: A) => (s"/tmp/snippet $a": ProcessBuilder).lines.foreach(Console.println _)
  }
  def eval(a: A): Unit = { val f1 = f; time("eval")(f1(a)) }
}

// STUB CODE

trait Base extends EmbeddedControls with OverloadHack with lms.util.ClosureCompare {
  type Rep[+T] = Exp[T];
  abstract class Exp[+T]
  abstract class Def[+T]
  abstract class Var[T]
  abstract class Block[T]

  def Wrap[A:Manifest](x: lms.core.Backend.Exp): Exp[A] = {
    if (manifest[A] == manifest[Unit]) Const(()).asInstanceOf[Exp[A]]
    else new Wrap[A](x)
  }
  case class Wrap[+A:Manifest](x: lms.core.Backend.Exp) extends Exp[A] {
    Adapter.typeMap(x) = manifest[A]
  }
  def Unwrap(x: Exp[Any]) = x match {
    case Wrap(x) => x
    case Const(x) => Backend.Const(x)
  }

  case class WrapV[A:Manifest](x: lms.core.Backend.Exp) extends Var[A] {
    Adapter.typeMap(x) = manifest[A] // could include Var type in manifest
  }
  def UnwrapV[T](x: Var[T]) = x match { case WrapV(x) => x }

  case class Const[T](x: T) extends Exp[T]
  case class Sym[T](x: Int) extends Exp[T]

  def convertToExp(x: Any) = x match {
    case e: Exp[Any] => Unwrap(e)
    case c => Backend.Const(c)
  }

  implicit def unit[T:Manifest](x: T): Rep[T] = Wrap[T](Backend.Const(x))
  implicit def toAtom[T:Manifest](x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflect(p.productPrefix, xs:_*))
  }

  def staticData[T:Manifest](x: T): Rep[T] =
    Wrap[T](Adapter.g.reflect("staticData", Backend.Const(x)))


  def reflectEffect[T:Manifest](x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectEffect(p.productPrefix, xs:_*)(Adapter.CTRL))
  }
  def reflectMutable[T:Manifest](x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectEffect(p.productPrefix, xs:_*)(Adapter.STORE))
  }
  def reflectWrite[T:Manifest](w: Rep[Any])(x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectEffect(p.productPrefix, xs:_*)(Unwrap(w)))
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit = ???

  // Former Ops:

/*
  def FUN(f: INT => INT): INT => INT = FUN((_,x) => f(x))

  def FUN(f: ((INT=>INT),INT) => INT): INT => INT = {
    val fn = Sym(g.fresh)
    //val xn = Sym(g.fresh)
    val f1 = (x: INT) => APP(fn,x)
    // NOTE: lambda expression itself does not have
    // an effect, so body block should not count as
    // latent effect of the lambda
    g.reflect(fn,"λ",g.reify(xn => f(f1,INT(xn)).x))()()
    f1
  }

  var funTable: List[(Sym[_], Any)] = List()
  override def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B])(implicit pos: SourceContext): Exp[A => B] = {
    val can = canonicalize(f)
    funTable.find(_._2 == can) match {
      case Some((funSym, _)) =>
        funSym.asInstanceOf[Exp[A=>B]]
      case _ =>
        val funSym = fresh[A=>B]
        funTable = (funSym,can)::funTable
        createDefinition(funSym, doLambdaDef(f))
        funSym
    }
  }
*/



  // def compile[A,B](f: Rep[A] => Rep[B]): A=>B = ???
  // def compile[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): A=>B = {
  //   val (src, statics) = Adapter.emitScala("Snippet")(manifest[A],manifest[B])(x => Unwrap(f(Wrap(x))))
  //   val fc = time("scalac") { Global.sc.compile[A,B]("Snippet", src, statics.toList) }
  //   fc
  // }

  class SeqOpsCls[T](x: Rep[Seq[Char]])

  // implemented in trait Equal
  def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext) : Rep[Boolean]
  def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext) : Rep[Boolean]

  // ArrayOps
  // XXX HACK for generic type!
  def NewArray[T:Manifest](x: Rep[Int]): Rep[Array[T]] = Wrap[Array[T]](Adapter.g.reflectEffect("NewArray", Unwrap(x))(Adapter.STORE))
  def Array[T:Manifest](xs: Rep[T]*): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectEffect("Array", xs.map(Unwrap(_)):_*)(Adapter.STORE))
  }
  implicit class ArrayOps[A:Manifest](x: Rep[Array[A]]) {
    def apply(i: Rep[Int]): Rep[A] = Wrap[A](Adapter.g.reflectEffect("array_get", Unwrap(x), Unwrap(i))(Unwrap(x)))
    def update(i: Rep[Int], y: Rep[A]): Unit = Wrap[Unit](Adapter.g.reflectEffect("array_set", Unwrap(x), Unwrap(i), Unwrap(y))(Unwrap(x)))
    def length: Rep[Int] = Wrap[Int](Adapter.g.reflect("Array.length", Unwrap(x)))
    def slice(s: Rep[Int], e: Rep[Int]): Rep[Array[A]] = Wrap[Array[A]](Adapter.g.reflect("Array.slice", Unwrap(s), Unwrap(e)))
  }

  trait LongArray[+T]
  def NewLongArray[T:Manifest](x: Rep[Long], init: Option[Int] = None): Rep[LongArray[T]] = Wrap[LongArray[T]](Adapter.g.reflectEffect("NewArray", Unwrap(x))(Adapter.STORE))
  implicit class LongArrayOps[A:Manifest](x: Rep[LongArray[A]]) {
    def apply(i: Rep[Long]): Rep[A] = Wrap[A](Adapter.g.reflectEffect("array_get", Unwrap(x), Unwrap(i))(Unwrap(x)))
    def update(i: Rep[Long], y: Rep[A]): Unit = Adapter.g.reflectEffect("array_set", Unwrap(x), Unwrap(i), Unwrap(y))(Unwrap(x))
    def length: Rep[Long] = Wrap[Long](Adapter.g.reflect("Array.length", Unwrap(x)))
    def slice(s: Rep[Long], e: Rep[Long] = unit(0L)): Rep[LongArray[A]] = Wrap[LongArray[A]](Adapter.g.reflect("+", Unwrap(x), Unwrap(s)))
  }

  // BooleanOps
  implicit def bool2boolOps(lhs: Boolean) = new BoolOps(lhs)
  implicit def var2boolOps(lhs: Var[Boolean]) = new BoolOps(lhs)
  implicit class BoolOps(lhs: Rep[Boolean]) {
    def unary_!(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect("Boolean.!", Unwrap(lhs)))
    def &&(rhs: =>Rep[Boolean])(implicit pos: SourceContext) =
    __ifThenElse(lhs, rhs, unit(false))
    // def &&(rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean] = lhs match {
    //   case Wrap(Backend.Const(true))  => rhs
    //   case Wrap(Backend.Const(false)) => unit(false)
    //   case _ => Wrap[Boolean](Adapter.g.reflect("&&", Unwrap(lhs), Unwrap(rhs)))
    // }
    // def ||(rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean] = lhs match {
    //   case Wrap(Backend.Const(true))  => unit(true)
    //   case Wrap(Backend.Const(false)) => rhs
    //   case _ => Wrap[Boolean](Adapter.g.reflect("||", Unwrap(lhs), Unwrap(rhs)))
    // }
    def ||(rhs: =>Rep[Boolean])(implicit pos: SourceContext) =
      __ifThenElse(lhs, unit(true), rhs)
  }

  // shift/reset
  def shift1[A:Manifest](f: Rep[A => Unit] => Rep[Unit]): Rep[A] = {
    val bBlock = Adapter.g.reify(x => Unwrap(f(Wrap[A => Unit](x))))
    val efs = Adapter.g.getEffKeys(bBlock)
    Wrap[A](Adapter.g.reflectEffect("shift1", bBlock)(efs: _*))
  }
  def reset1(f: => Rep[Unit]): Rep[Unit] = {
    val rBlock = Adapter.g.reify(Unwrap(f))
    val efs = Adapter.g.getEffKeys(rBlock)
    Wrap[Unit](Adapter.g.reflectEffect("reset1", rBlock)(efs: _*))
  }

  // Functions
  def fun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A => B] =
    Wrap[A=>B](__fun(f, 1, xn => Unwrap(f(Wrap[A](xn(0))))))

  def doLambda[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A => B] = fun(f)
  implicit class FunOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] =
      Wrap[B](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x))(Adapter.CTRL))
  }

  def fun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] =
    Wrap[(A,B)=>C](__fun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] = fun(f)
  implicit class FunOps2[A:Manifest,B:Manifest,C:Manifest](f: Rep[(A,B) => C]) {
    def apply(x: Rep[A], y: Rep[B]): Rep[C] =
      Wrap[C](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y))(Adapter.CTRL))
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] =
    Wrap[(A,B,C)=>D](__fun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] = fun(f)
  implicit class FunOps3[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: Rep[(A,B,C) => D]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C]): Rep[D] =
      Wrap[D](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z))(Adapter.CTRL))
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] =
    Wrap[(A,B,C,D)=>E](__fun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] = fun(f)
  implicit class FunOps4[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: Rep[(A,B,C,D) => E]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D]): Rep[E] =
      Wrap[E](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w))(Adapter.CTRL))
  }

  def __fun[T:Manifest](f: AnyRef, arity: Int, gf: List[Backend.Exp] => Backend.Exp): Backend.Exp = {
    val can = canonicalize(f)
    Adapter.funTable.find(_._2 == can) match {
      case Some((funSym, _)) => funSym
      case _ =>
        val fn = Backend.Sym(Adapter.g.fresh)
        Adapter.funTable = (fn, can)::Adapter.funTable

        val fn1 = Backend.Sym(Adapter.g.fresh)
        Adapter.g.reflect(fn, "λforward", fn1)()()
        //val xn = Sym(g.fresh)
        //val f1 = (x: INT) => APP(fn,x)
        // NOTE: lambda expression itself does not have
        // an effect, so body block should not count as
        // latent effect of the lambda
        val res = Adapter.g.reflect(fn1,"λ",Adapter.g.reify(arity, gf))(fn)()
        Adapter.funTable = Adapter.funTable.map {
          case (fn2, can2) => if (can == can2) (fn1, can) else (fn2, can2)
        }
        res
    }
  }
  // Top-Level Functions
  // XXX: is this data structure needed? should it move elsewhere?
  // could we use funTable instead?
  val topLevelFunctions = new scala.collection.mutable.HashMap[AnyRef,Backend.Exp]()
  def __topFun(f: AnyRef, arity: Int, gf: List[Backend.Exp] => Backend.Exp): Backend.Exp = {
    val can = canonicalize(f)
    topLevelFunctions.getOrElseUpdate(can, {
      val fn = Backend.Sym(Adapter.g.fresh)
      Adapter.g.reflect(fn,"λtop", Adapter.g.reify(arity, gf))()()
    })
  }
  def topFun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A => B] =
    Wrap[A=>B](__topFun(f, 1, xn => Unwrap(f(Wrap[A](xn(0))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] =
    Wrap[(A,B)=>C](__topFun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] =
    Wrap[(A,B,C)=>D](__topFun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] =
    Wrap[(A,B,C,D)=>E](__topFun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3))))))

  // IfThenElse
  def __ifThenElse[T:Manifest](c: Rep[Boolean], a: => Rep[T], b: => Rep[T])(implicit pos: SourceContext): Rep[T] = c match {
    case Wrap(Backend.Const(true))  => a
    case Wrap(Backend.Const(false)) => b
    case _ =>
      Wrap[T](Adapter.IF(Adapter.BOOL(Unwrap(c)))
                     (Adapter.INT(Unwrap(a)))
                     (Adapter.INT(Unwrap(b))).x)
  }

  // ImplicitOps
  // TR: I'm concerned about this because it is overly general -- it doesn't appear to be used anyways?
  // def implicit_convert[A:Manifest,B:Manifest](a: Rep[A]): Rep[B] = Wrap[B](Adapter.g.reflect("convert", Unwrap(a)))

  // MiscOps
  def exit(res: Rep[Int]): Unit = Adapter.g.reflectEffect("exit", Unwrap(res))(Adapter.CTRL)
  def println(x: Rep[Any]): Unit =
    Adapter.g.reflectEffect("P",Unwrap(x))(Adapter.CTRL)

  def printf(f: String, x: Rep[Any]*): Unit = {
    Adapter.g.reflectEffect("printf",Backend.Const(f)::x.map(Unwrap).toList:_*)(Adapter.CTRL)
  }

  // TimingOps
  def timeGeneratedCode[A: Manifest](f: => Rep[A], msg: Rep[String] = unit("")): Rep[A] = {
    val ff = Adapter.g.reify(Unwrap(f))
    Wrap[A](Adapter.g.reflectEffect("timeGenerated", Unwrap(msg), ff)(Adapter.g.getEffKeys(ff):_*))
  }
  def timestamp: Rep[Long] = Wrap[Long](Adapter.g.reflectEffect("timestamp")(Adapter.CTRL))


  // case class GenerateComment(l: String) extends Def[Unit]
  // case class Comment[A:Manifest](l: String, verbose: Boolean, b: Block[A]) extends Def[A]
  def generate_comment(l: String): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectEffect("generate-comment", Backend.Const(l))(Adapter.CTRL))
  }
  def comment[A:Manifest](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A] = {
    val g = Adapter.g
    val bb = g.reify(Unwrap(b))
    if (g.isPure(bb))
      Wrap[A](g.reflect("comment",Backend.Const(l),Backend.Const(verbose),bb))
    else
      Wrap[A](g.reflectEffect("comment",Backend.Const(l),Backend.Const(verbose),bb)(g.getEffKeys(bb):_*))
  }

  // RangeOps
  // NOTE(trans): it has to be called 'intWrapper' to shadow the standard Range constructor
  implicit class intWrapper(start: Int) {
    // Note that these are ambiguous - need to use type ascription here (e.g. 1 until 10 : Rep[Range])

    def until(end: Rep[Int])(implicit pos: SourceContext) = range_until(unit(start),end)
    def until(end: Int)(implicit pos: SourceContext): Rep[Range] = range_until(unit(start),unit(end))

    def until(end: Int)(implicit pos: SourceContext, o: Overloaded1): Range = new Range(start,end,1)
  }
  def range_until(start: Rep[Int], end: Rep[Int]): Rep[Range] = {
    Wrap[Range](Adapter.g.reflect("range_until", Unwrap(start), Unwrap(end)))
  }

  implicit class RangeConstrOps(lhs: Rep[Int]) {
    def until(y: Rep[Int]): Rep[Range] = range_until(lhs,y)

    // unrelated
    def ToString: Rep[String] =
      Wrap[String](Adapter.g.reflect("Object.toString", Unwrap(lhs)))
  }

  implicit class RangeOps(lhs: Rep[Range]) {
    def foreach(f: Rep[Int] => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit] = {

      // XXX TODO: it would be good to do this as lowering/codegen
      // (as done previously in LMS), but for now just construct
      // a while loop directly

      val Adapter.g.Def("range_until", List(x0:Backend.Exp,x1:Backend.Exp)) = Unwrap(lhs)

      // val b = Adapter.g.reify(i => Unwrap(f(Wrap(i))))
      // val f1 = Adapter.g.reflect("λ",b)
      // Wrap(Adapter.g.reflect("range_foreach", x0, x1, b))

      val i = var_new(Wrap[Int](x0))
      __whileDo(notequals(readVar(i), Wrap[Int](x1)), {
        f(readVar(i))
        i += 1
      })
    }
  }

  trait LongRange
  implicit class longWrapper(start: Long) {
    // Note that these are ambiguous - need to use type ascription here (e.g. 1 until 10 : Rep[LongRange])

    def until(end: Rep[Long])(implicit pos: SourceContext) = longrange_until(unit(start),end)
    def until(end: Long)(implicit pos: SourceContext): Rep[LongRange] = longrange_until(unit(start),unit(end))

    def until(end: Long)(implicit pos: SourceContext, o: Overloaded1): immutable.NumericRange.Exclusive[Long] = new immutable.NumericRange.Exclusive(start,end,1L)
  }
  def longrange_until(start: Rep[Long], end: Rep[Long]): Rep[LongRange] = {
    Wrap[LongRange](Adapter.g.reflect("longrange_until", Unwrap(start), Unwrap(end)))
  }

  implicit class LongRangeConstrOps(lhs: Rep[Long]) {
    def until(y: Rep[Long]): Rep[LongRange] = longrange_until(lhs,y)

    // unrelated
    def ToString: Rep[String] =
      Wrap[String](Adapter.g.reflect("Object.toString", Unwrap(lhs)))
  }

  implicit class LongRangeOps(lhs: Rep[LongRange]) {
    def foreach(f: Rep[Long] => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit] = {

      // XXX TODO: it would be good to do this as lowering/codegen
      // (as done previously in LMS), but for now just construct
      // a while loop directly

      val Adapter.g.Def("longrange_until", List(x0:Backend.Exp,x1:Backend.Exp)) = Unwrap(lhs)

      // val b = Adapter.g.reify(i => Unwrap(f(Wrap(i))))
      // val f1 = Adapter.g.reflect("λ",b)
      // Wrap(Adapter.g.reflect("range_foreach", x0, x1, b))

      val i = var_new(Wrap[Long](x0))
      __whileDo(notequals(readVar(i), Wrap[Long](x1)), {
        f(readVar(i))
        i += 1L
      })
    }
  }

  // StringOps
  implicit class StringOps(lhs: Rep[String]) {
    def charAt(i: Rep[Int]): Rep[Char] = Wrap[Char](Adapter.g.reflect("String.charAt", Unwrap(lhs), Unwrap(i))) // XXX: may fail! effect?
    def apply(i: Rep[Int]): Rep[Char] = charAt(i)
    def length: Rep[Int] = Wrap[Int](Adapter.g.reflect("String.length", Unwrap(lhs)))
    def toInt: Rep[Int] = Wrap[Int](Adapter.g.reflect("String.toInt", Unwrap(lhs))) // XXX: may fail!
    def toDouble: Rep[Double] = Wrap[Double](Adapter.g.reflect("String.toDouble", Unwrap(lhs))) // XXX: may fail!
  }

  // UncheckedOps
  private def uncheckedHelp(xs: Seq[Any]) = {
    (xs map {
      case s: String => s
      case e: Exp[Any] => "[ ]"
      case e: Var[Any] => "[ ]"
      case e => e.toString
    } mkString(""),
    xs collect {
      case e: Exp[Any] => Unwrap(e)
      case v: Var[Any] => UnwrapV(v)
    })
  }
  def unchecked[T:Manifest](xs: Any*): Rep[T] = {
    val (strings, args) = uncheckedHelp(xs)
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args:_*)(Adapter.CTRL))
  }
  def uncheckedPure[T:Manifest](xs: Any*): Rep[T] = {
    val (strings, args) = uncheckedHelp(xs)
    Wrap[T](Adapter.g.reflect("unchecked" + strings, args:_*))
  }

  // Variables
  implicit def readVar[T:Manifest](x: Var[T]): Rep[T] = Wrap[T](Adapter.g.reflectEffect("var_get", UnwrapV(x))(UnwrapV(x)))
  def var_new[T:Manifest](x: Rep[T]): Var[T] = WrapV[T](Adapter.g.reflectEffect("var_new", Unwrap(x))(Adapter.STORE))
  def __assign[T:Manifest](lhs: Var[T], rhs: Rep[T]): Unit = Wrap[Unit](Adapter.g.reflectEffect("var_set", UnwrapV(lhs), Unwrap(rhs))(UnwrapV(lhs)))
  def __assign[T:Manifest](lhs: Var[T], rhs: Var[T]): Unit = __assign(lhs,readVar(rhs))
  def __assign[T:Manifest](lhs: Var[T], rhs: T): Unit = __assign(lhs,unit(rhs)) // shouldn't unit lift T to Rep[T] implicitly?


  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T] =
    Wrap[T]((Adapter.INT(Unwrap(lhs)) + Adapter.INT(Unwrap(rhs))).x) // XXX: not distinguishing types here ...
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T] =
    Wrap[T]((Adapter.INT(Unwrap(lhs)) - Adapter.INT(Unwrap(rhs))).x) // XXX: not distinguishing types here ...
  def numeric_mult[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T] =
    Wrap[T]((Adapter.INT(Unwrap(lhs)) * Adapter.INT(Unwrap(rhs))).x) // XXX: not distinguishing types here ...

  implicit class OpsInfixVarT[T:Manifest:Numeric](lhs: Var[T]) {
    def +=(rhs: T): Unit = __assign(lhs,numeric_plus(readVar(lhs),rhs))
    def +=(rhs: Rep[T]): Unit = __assign(lhs,numeric_plus(readVar(lhs),rhs))
    def +=(rhs: Var[T]): Unit = __assign(lhs,numeric_plus(readVar(lhs),readVar(rhs)))
    def -=(rhs: T): Unit = __assign(lhs,numeric_minus(readVar(lhs),rhs))
    def -=(rhs: Rep[T]): Unit = __assign(lhs,numeric_minus(readVar(lhs),rhs))
    def -=(rhs: Var[T]): Unit = __assign(lhs,numeric_minus(readVar(lhs),readVar(rhs)))
    def *=(rhs: T): Unit = __assign(lhs,numeric_mult(readVar(lhs),rhs))
    def *=(rhs: Rep[T]): Unit = __assign(lhs,numeric_mult(readVar(lhs),rhs))
    def *=(rhs: Var[T]): Unit = __assign(lhs,numeric_mult(readVar(lhs),readVar(rhs)))
  }

  // While
  def __whileDo(c: => Rep[Boolean], b: => Rep[Unit]): Rep[Unit] = {
      Adapter.WHILE(Adapter.BOOL(Unwrap(c)))(b)
  }
  def __whileDoInternal(c: => Rep[Boolean], b: => Rep[Unit]): Rep[Unit] = {
      Adapter.WHILE(Adapter.BOOL(Unwrap(c)))(b)
  }

}

trait Compile
trait CompileScala

trait BaseExp
trait ScalaGenBase extends ExtendedScalaCodeGen {
  val IR: Base
  import IR._
  implicit class CodegenHelper(sc: StringContext) {
    def src(args: Any*): String = ???
  }
  // def remap[A](m: Manifest[A]): String = ???
  // def quote(x: Exp[Any]) : String = ???
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = ???
  def emitSource[A : Manifest, B : Manifest](f: Rep[A]=>Rep[B], className: String, stream: java.io.PrintStream): List[(Class[_], Any)] = {
    val statics = Adapter.emitCommon1(className,this,stream)(manifest[A],manifest[B])(x => Unwrap(f(Wrap[A](x))))
    // stream.println(src)
    statics.toList
  }
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: java.io.PrintStream): List[(Sym[Any], Any)] = ???
}
trait CGenBase extends ExtendedCCodeGen {
  val IR: Base
  import IR._
  // def remap[A](m: Manifest[A]): String = ???
  // def quote(x: Exp[Any]) : String = ???
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = ???
  def emitSource[A : Manifest, B : Manifest](f: Rep[A]=>Rep[B], className: String, stream: java.io.PrintStream): List[(Class[_], Any)] = {
    val statics = Adapter.emitCommon1(className,this,stream)(manifest[A],manifest[B])(x => Unwrap(f(Wrap[A](x))))
    // stream.println(src)
    statics.toList
  }
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: java.io.PrintStream): List[(Sym[Any], Any)] = ???
}

// trait PrimitiveOps
trait NumericOps
trait BooleanOps
trait LiftString
// trait LiftPrimitives
trait LiftNumeric
trait LiftBoolean
trait IfThenElse
// trait Equal
trait RangeOps
// trait OrderingOps
trait MiscOps
trait ArrayOps
trait StringOps
trait SeqOps
trait Functions
trait While
trait StaticData
trait Variables
// trait LiftVariables
trait ObjectOps
// trait UtilOps
trait UncheckedOps
trait Timing

trait PrimitiveOpsExpOpt
trait NumericOpsExpOpt
trait BooleanOpsExp
trait IfThenElseExpOpt
trait EqualExpBridgeOpt
trait RangeOpsExp
trait OrderingOpsExp
trait MiscOpsExp
trait EffectExp
trait ArrayOpsExpOpt
trait StringOpsExp
trait SeqOpsExp
trait FunctionsRecursiveExp
trait WhileExp
trait StaticDataExp
trait VariablesExpOpt
trait ObjectOpsExpOpt
// trait UtilOpsExp
trait UncheckedOpsExp

trait ScalaGenNumericOps extends ScalaGenBase
trait ScalaGenPrimitiveOps extends ScalaGenBase
trait ScalaGenBooleanOps extends ScalaGenBase
trait ScalaGenIfThenElse extends ScalaGenBase
trait ScalaGenEqual extends ScalaGenBase
trait ScalaGenRangeOps extends ScalaGenBase
trait ScalaGenOrderingOps extends ScalaGenBase
trait ScalaGenMiscOps extends ScalaGenBase
trait ScalaGenArrayOps extends ScalaGenBase
trait ScalaGenStringOps extends ScalaGenBase
trait ScalaGenSeqOps extends ScalaGenBase
trait ScalaGenFunctions extends ScalaGenBase
trait ScalaGenWhile extends ScalaGenBase
trait ScalaGenStaticData extends ScalaGenBase
trait ScalaGenVariables extends ScalaGenBase
trait ScalaGenObjectOps extends ScalaGenBase
trait ScalaGenUtilOps extends ScalaGenBase
trait ScalaGenUncheckedOps extends ScalaGenBase
trait ScalaGenEffect extends ScalaGenBase

trait CGenNumericOps
trait CGenPrimitiveOps
trait CGenBooleanOps
trait CGenIfThenElse
trait CGenEqual
trait CGenRangeOps
trait CGenOrderingOps
trait CGenMiscOps
trait CGenArrayOps
trait CGenStringOps
trait CGenSeqOps
trait CGenFunctions
trait CGenWhile
trait CGenStaticData
trait CGenVariables
trait CGenObjectOps
trait CGenUtilOps
trait CGenUncheckedOps


trait LiftVariables extends Base {
  def __newVar[T:Manifest](init: T)(implicit pos: SourceContext) = var_new(unit(init))
  def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_new(init)
  def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_new(init)
}


trait Equal extends Base {
  def infix_==[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a,b)
  def infix_==[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded2, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a, b)
  def infix_==[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded3, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a, b)
  def infix_==[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(a, unit(b))
  def infix_==[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(unit(a), b)
  def infix_==[A,B](a: Var[A], b: B)(implicit o: Overloaded6, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(a, unit(b))
  def infix_==[A,B](a: A, b: Var[B])(implicit o: Overloaded7, mA: Manifest[A], mB: Manifest[B], pos: SourceContext): Rep[Boolean] = equals(unit(a), b)
  def infix_==[A,B](a: Var[A], b: Var[B])(implicit o: Overloaded8, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = equals(a,b)

  def infix_!=[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a,b)
  def infix_!=[A,B](a: Rep[A], b: Var[B])(implicit o: Overloaded2, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Var[A], b: Rep[B])(implicit o: Overloaded3, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, b)
  def infix_!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, unit(b))
  def infix_!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(unit(a), b)
  def infix_!=[A,B](a: Var[A], b: B)(implicit o: Overloaded6, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a, unit(b))
  def infix_!=[A,B](a: A, b: Var[B])(implicit o: Overloaded7, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(unit(a), b)
  def infix_!=[A,B](a: Var[A], b: Var[B])(implicit o: Overloaded8, mA: Manifest[A], mB: Manifest[B], pos: SourceContext) : Rep[Boolean] = notequals(a,b)

  def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext) : Rep[Boolean] =
    Wrap[Boolean](Adapter.g.reflect("==",Unwrap(a),Unwrap(b)))
  def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B])(implicit pos: SourceContext) : Rep[Boolean] =
    Wrap[Boolean](Adapter.g.reflect("!=",Unwrap(a),Unwrap(b)))
}

trait OrderingOps extends Base with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  implicit def orderingToOrderingOps[T:Ordering:Manifest](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:Manifest](n: Rep[T]) = new OrderingOpsCls(n)
  implicit def varOrderingToOrderingOps[T:Ordering:Manifest](n: Var[T]) = new OrderingOpsCls(readVar(n))

  class OrderingOpsCls[T:Ordering:Manifest](lhs: Rep[T]){
    def <       (rhs: Rep[T])(implicit pos: SourceContext) = ordering_lt(lhs, rhs)
    def <=      (rhs: Rep[T])(implicit pos: SourceContext) = ordering_lteq(lhs, rhs)
    def >       (rhs: Rep[T])(implicit pos: SourceContext) = ordering_gt(lhs, rhs)
    def >=      (rhs: Rep[T])(implicit pos: SourceContext) = ordering_gteq(lhs, rhs)
    def equiv   (rhs: Rep[T])(implicit pos: SourceContext) = ordering_equiv(lhs, rhs)
    def max     (rhs: Rep[T])(implicit pos: SourceContext) = ordering_max(lhs, rhs)
    def min     (rhs: Rep[T])(implicit pos: SourceContext) = ordering_min(lhs, rhs)
    def compare (rhs: Rep[T])(implicit pos: SourceContext) = ordering_compare(lhs, rhs)

    def <       [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_lt(lhs, c(rhs))
    def <=      [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_lteq(lhs, c(rhs))
    def >       [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_gt(lhs, c(rhs))
    def >=      [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_gteq(lhs, c(rhs))
    def equiv   [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_equiv(lhs, c(rhs))
    def max     [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_max(lhs, c(rhs))
    def min     [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_min(lhs, c(rhs))
    def compare [B](rhs: B)(implicit c: B => Rep[T], pos: SourceContext) = ordering_compare(lhs, c(rhs))
  }

  def ordering_lt      [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect("<", Unwrap(lhs), Unwrap(rhs)))
  def ordering_lteq    [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect("<=", Unwrap(lhs), Unwrap(rhs)))
  def ordering_gt      [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect(">", Unwrap(lhs), Unwrap(rhs)))
  def ordering_gteq    [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect(">=", Unwrap(lhs), Unwrap(rhs)))
  def ordering_equiv   [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect("==", Unwrap(lhs), Unwrap(rhs)))
  def ordering_max     [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]       = Wrap[T]      (Adapter.g.reflect("max", Unwrap(lhs), Unwrap(rhs)))
  def ordering_min     [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]       = Wrap[T]      (Adapter.g.reflect("min", Unwrap(lhs), Unwrap(rhs)))
  def ordering_compare [T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[Int]     = Wrap[Int]    (Adapter.g.reflect("compare", Unwrap(lhs), Unwrap(rhs)))
}


trait LiftPrimitives {
  this: PrimitiveOps =>

  implicit def intToRepInt(x: Int)(implicit __pos: SourceContext) = unit(x)
  implicit def floatToRepFloat(x: Float)(implicit __pos: SourceContext) = unit(x)
  implicit def doubleToRepDouble(x: Double)(implicit __pos: SourceContext) = unit(x)
  implicit def longToRepLong(x: Long)(implicit __pos: SourceContext) = unit(x)

  // precision-widening promotions
  implicit def chainIntToRepFloat[A:Manifest](x: A)(implicit c: A => Rep[Int], __pos: SourceContext): Rep[Float] = repIntToRepFloat(c(x))
  implicit def chainFloatToRepDouble[A:Manifest](x: A)(implicit c: A => Rep[Float], __pos: SourceContext): Rep[Double] = repFloatToRepDouble(c(x))
}

/**
 * This file is extremely boilerplate. In fact, most of the code here is copied from a
 * Forge-generated file. We need a static version since Delite (and other projects) depend
 * on it without using Forge.
 */
trait PrimitiveOps extends Base with OverloadHack {

  /**
   * Primitive conversions
   */
  implicit def repIntToRepDouble(x: Rep[Int])(implicit __pos: SourceContext): Rep[Double] = x.toDouble
  implicit def repIntToRepFloat(x: Rep[Int])(implicit __pos: SourceContext): Rep[Float] = x.toFloat
  implicit def repIntToRepLong(x: Rep[Int])(implicit __pos: SourceContext): Rep[Long] = x.toLong
  implicit def repFloatToRepDouble(x: Rep[Float])(implicit __pos: SourceContext): Rep[Double] = x.toDouble
  implicit def repLongToRepFloat(x: Rep[Long])(implicit __pos: SourceContext): Rep[Float] = x.toFloat
  implicit def repLongToRepDouble(x: Rep[Long])(implicit __pos: SourceContext): Rep[Double] = x.toDouble
  implicit def repCharToRepInt(x: Rep[Char])(implicit __pos: SourceContext): Rep[Int] = x.toInt

  /**
   * Enumerate all combinations of primitive math.
   * Avoids certain fragile behavior, including compiler crashes and some erroneous or inaccessible type errors.
   */


  // -- BEGIN FORGE-GENERATED SECTION

  implicit def repToPrimitiveMathOpsDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext) = new PrimitiveMathOpsDoubleOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsDoubleOpsCls(x: Double)(implicit __pos: SourceContext) = new PrimitiveMathOpsDoubleOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsDoubleOpsCls(x: Var[Double])(implicit __pos: SourceContext) = new PrimitiveMathOpsDoubleOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def unary_- = Wrap[Double](Adapter.g.reflect("-", Unwrap(self)))

    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { double_plus(self, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { double_minus(self, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { double_times(self, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_divide(self, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_plus(self, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_minus(self, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { double_times(self, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_divide(self, rhs) }

    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_plus(self, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_minus(self, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { double_times(self, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_divide(self, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_divide(self, rhs.toDouble) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_divide(self, readVar(rhs).toDouble) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_divide(self, rhs.toDouble) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_divide(self, readVar(rhs).toDouble) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_plus(self, unit(rhs.toDouble)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_minus(self, unit(rhs.toDouble)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { double_times(self, unit(rhs.toDouble)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_divide(self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_plus(self, rhs.toDouble) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_minus(self, rhs.toDouble) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { double_times(self, rhs.toDouble) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_divide(self, rhs.toDouble) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_plus(self, readVar(rhs).toDouble) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_minus(self, readVar(rhs).toDouble) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { double_times(self, readVar(rhs).toDouble) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded21) = { double_divide(self, readVar(rhs).toDouble) }
  }

  implicit def repToPrimitiveMathOpsFloatOpsCls(x: Rep[Float])(implicit __pos: SourceContext) = new PrimitiveMathOpsFloatOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsFloatOpsCls(x: Float)(implicit __pos: SourceContext) = new PrimitiveMathOpsFloatOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsFloatOpsCls(x: Var[Float])(implicit __pos: SourceContext) = new PrimitiveMathOpsFloatOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsFloatOpsCls(val self: Rep[Float])(implicit __pos: SourceContext) {
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_divide(self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded44) = { double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { float_plus(self, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { float_minus(self, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { float_times(self, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_divide(self, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_plus(self, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_minus(self, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { float_times(self, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_divide(self, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_plus(self, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_minus(self, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { float_times(self, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_divide(self, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_plus(self, unit(rhs.toFloat)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_minus(self, unit(rhs.toFloat)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { float_times(self, unit(rhs.toFloat)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_divide(self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_plus(self, rhs.toFloat) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_minus(self, rhs.toFloat) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { float_times(self, rhs.toFloat) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_divide(self, rhs.toFloat) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_plus(self, readVar(rhs).toFloat) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_minus(self, readVar(rhs).toFloat) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { float_times(self, readVar(rhs).toFloat) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_divide(self, readVar(rhs).toFloat) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_plus(self, unit(rhs.toFloat)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_minus(self, unit(rhs.toFloat)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { float_times(self, unit(rhs.toFloat)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_divide(self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_plus(self, rhs.toFloat) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_minus(self, rhs.toFloat) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { float_times(self, rhs.toFloat) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_divide(self, rhs.toFloat) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_plus(self, readVar(rhs).toFloat) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_minus(self, readVar(rhs).toFloat) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { float_times(self, readVar(rhs).toFloat) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded53) = { float_divide(self, readVar(rhs).toFloat) }
  }

  implicit def repToPrimitiveMathOpsCharOpsCls(x: Rep[Char])(implicit __pos: SourceContext) = new PrimitiveMathOpsCharOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsCharOpsCls(x: Char)(implicit __pos: SourceContext) = new PrimitiveMathOpsCharOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsCharOpsCls(x: Var[Char])(implicit __pos: SourceContext) = new PrimitiveMathOpsCharOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsCharOpsCls(val self: Rep[Char])(implicit __pos: SourceContext) {
    //STUB
    def -(rhs: Char)(implicit __pos: SourceContext,__imp1: Overloaded73): Rep[Char] = char_minus(self, rhs)
    def -(rhs: Rep[Char])(implicit __pos: SourceContext,__imp1: Overloaded73): Rep[Char] = char_minus(self, rhs)

  }

  def char_minus(lhs: Rep[Char], rhs: Rep[Char])(implicit pos: SourceContext): Rep[Char] =
    Wrap[Char]((Adapter.INT(Unwrap(lhs)) - Adapter.INT(Unwrap(rhs))).x)


  implicit def repToPrimitiveMathOpsIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext) = new PrimitiveMathOpsIntOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsIntOpsCls(x: Int)(implicit __pos: SourceContext) = new PrimitiveMathOpsIntOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsIntOpsCls(x: Var[Int])(implicit __pos: SourceContext) = new PrimitiveMathOpsIntOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def unary_- = Wrap[Int](Adapter.g.reflect("-", Unwrap(self)))

    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_divide(self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded76) = { double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { float_plus(self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { float_minus(self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { float_times(self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_divide(self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_plus(self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_minus(self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { float_times(self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_divide(self.toFloat, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_plus(self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_minus(self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { float_times(self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded79) = { float_divide(self.toFloat, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { int_plus(self, unit(rhs)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { int_minus(self, unit(rhs)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { int_times(self, unit(rhs)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_divide(self, unit(rhs)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_plus(self, rhs) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_minus(self, rhs) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { int_times(self, rhs) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_divide(self, rhs) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_plus(self, readVar(rhs)) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_minus(self, readVar(rhs)) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { int_times(self, readVar(rhs)) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded82) = { int_divide(self, readVar(rhs)) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { long_plus(self.toLong, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { long_minus(self.toLong, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { long_times(self.toLong, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_divide(self.toLong, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_plus(self.toLong, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_minus(self.toLong, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { long_times(self.toLong, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_divide(self.toLong, rhs) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_plus(self.toLong, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_minus(self.toLong, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { long_times(self.toLong, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded85) = { long_divide(self.toLong, readVar(rhs)) }
  }

  implicit def repToPrimitiveMathOpsLongOpsCls(x: Rep[Long])(implicit __pos: SourceContext) = new PrimitiveMathOpsLongOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsLongOpsCls(x: Long)(implicit __pos: SourceContext) = new PrimitiveMathOpsLongOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsLongOpsCls(x: Var[Long])(implicit __pos: SourceContext) = new PrimitiveMathOpsLongOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsLongOpsCls(val self: Rep[Long])(implicit __pos: SourceContext) {
    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { double_plus(self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { double_minus(self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { double_times(self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_divide(self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_plus(self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_minus(self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { double_times(self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_divide(self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_plus(self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_minus(self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { double_times(self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded108) = { double_divide(self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { float_plus(self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { float_minus(self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { float_times(self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_divide(self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_plus(self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_minus(self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { float_times(self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_divide(self.toFloat, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_plus(self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_minus(self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { float_times(self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded111) = { float_divide(self.toFloat, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { long_plus(self, unit(rhs.toLong)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { long_minus(self, unit(rhs.toLong)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { long_times(self, unit(rhs.toLong)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_divide(self, unit(rhs.toLong)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_plus(self, rhs.toLong) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_minus(self, rhs.toLong) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { long_times(self, rhs.toLong) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_divide(self, rhs.toLong) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_plus(self, readVar(rhs).toLong) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_minus(self, readVar(rhs).toLong) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { long_times(self, readVar(rhs).toLong) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_divide(self, readVar(rhs).toLong) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_plus(self, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_minus(self, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { long_times(self, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_divide(self, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_plus(self, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_minus(self, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { long_times(self, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_divide(self, rhs) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_plus(self, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_minus(self, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { long_times(self, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded117) = { long_divide(self, readVar(rhs)) }
  }

  object Math {
    def abs(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_abs(x)
    def abs(x: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded43): Rep[Float] = float_abs(x)
    def abs(x: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded44): Rep[Long] = long_abs(x)
    def abs(x: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded45): Rep[Int] = int_abs(x)

    def tanh(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_tanh(x)
    def sin(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_sin(x)
    def cos(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_cos(x)
    def exp(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_exp(x)
    def log(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_log(x)
    def sqrt(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = double_sqrt(x)
  }
  // -- END FORGE-GENERATED SECTION

  /**
   *  Double
   */

  object Double {
    def parseDouble(s:Rep[String])(implicit __pos: SourceContext) = obj_double_parseDouble(s)
    def PositiveInfinity(implicit pos: SourceContext) = obj_double_positive_infinity
    def NegativeInfinity(implicit pos: SourceContext) = obj_double_negative_infinity
    def MinValue(implicit pos: SourceContext) = obj_double_min_value
    def MaxValue(implicit pos: SourceContext) = obj_double_max_value
  }

  implicit def doubleToDoubleOps(n: Double): DoubleOpsCls = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]): DoubleOpsCls = new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps(n: Var[Double]): DoubleOpsCls = new DoubleOpsCls(readVar(n))

  class DoubleOpsCls(self: Rep[Double]) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_plus(self,unit(__arg1.toDouble)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_plus(self,unit(__arg1.toDouble)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_plus(self,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_plus(self,unit(__arg1.toDouble)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_plus(self,__arg1.toDouble) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_plus(self,__arg1.toDouble) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_plus(self,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_plus(self,__arg1.toDouble) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_plus(self,readVar(__arg1).toDouble) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_plus(self,readVar(__arg1).toDouble) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_plus(self,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_plus(self,readVar(__arg1).toDouble) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_minus(self,unit(__arg1.toDouble)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_minus(self,unit(__arg1.toDouble)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_minus(self,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_minus(self,unit(__arg1.toDouble)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_minus(self,__arg1.toDouble) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_minus(self,__arg1.toDouble) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_minus(self,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_minus(self,__arg1.toDouble) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_minus(self,readVar(__arg1).toDouble) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_minus(self,readVar(__arg1).toDouble) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_minus(self,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_minus(self,readVar(__arg1).toDouble) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_times(self,unit(__arg1.toDouble)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_times(self,unit(__arg1.toDouble)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_times(self,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded25) = { double_times(self,unit(__arg1.toDouble)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_times(self,__arg1.toDouble) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_times(self,__arg1.toDouble) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_times(self,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded34) = { double_times(self,__arg1.toDouble) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_times(self,readVar(__arg1).toDouble) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_times(self,readVar(__arg1).toDouble) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_times(self,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_times(self,readVar(__arg1).toDouble) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_divide(self,unit(__arg1.toDouble)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded22) = { double_divide(self,unit(__arg1.toDouble)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded23) = { double_divide(self,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded24) = { double_divide(self,unit(__arg1.toDouble)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_divide(self,__arg1.toDouble) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { double_divide(self,__arg1.toDouble) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_divide(self,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { double_divide(self,__arg1.toDouble) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded40) = { double_divide(self,readVar(__arg1).toDouble) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded41) = { double_divide(self,readVar(__arg1).toDouble) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded42) = { double_divide(self,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded43) = { double_divide(self,readVar(__arg1).toDouble) }
    def toInt(implicit pos: SourceContext) = double_to_int(self)
    def toFloat(implicit pos: SourceContext) = double_to_float(self)
    def toLong(implicit pos: SourceContext) = double_to_long(self)
  }

  def obj_double_parseDouble(s:Rep[String])(implicit pos: SourceContext): Rep[Double] = ???
  def obj_double_positive_infinity(implicit pos: SourceContext): Rep[Double] = ???
  def obj_double_negative_infinity(implicit pos: SourceContext): Rep[Double] = ???
  def obj_double_min_value(implicit pos: SourceContext): Rep[Double] = ???
  def obj_double_max_value(implicit pos: SourceContext): Rep[Double] = ???
  def double_plus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(lhs)) + Adapter.INT(Unwrap(rhs))).x)
  def double_minus(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(lhs)) - Adapter.INT(Unwrap(rhs))).x)
  def double_times(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(lhs)) * Adapter.INT(Unwrap(rhs))).x)
  def double_divide(lhs: Rep[Double], rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(lhs)) / Adapter.INT(Unwrap(rhs))).x)
  def double_sin(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).sin()).x)
  def double_cos(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).cos()).x)
  def double_tanh(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).tanh()).x)
  def double_abs(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).abs()).x)
  def double_exp(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).exp()).x)
  def double_log(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).log()).x)
  def double_sqrt(rhs: Rep[Double])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double]((Adapter.INT(Unwrap(rhs)).sqrt()).x)

  def double_to_int(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int](Adapter.g.reflect("cast", Unwrap(lhs)))
  def double_to_float(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float](Adapter.g.reflect("cast", Unwrap(lhs)))
  def double_to_long(lhs: Rep[Double])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long](Adapter.g.reflect("cast", Unwrap(lhs)))


  /**
   * Float
   */
  object Float {
    def parseFloat(s: Rep[String])(implicit pos: SourceContext) = obj_float_parse_float(s)
  }

  implicit def repToFloatOpsCls(x: Rep[Float])(implicit pos: SourceContext) = new FloatOpsCls(x)(pos)
  implicit def liftToFloatOpsCls(x: Float)(implicit pos: SourceContext) = new FloatOpsCls(unit(x))(pos)
  implicit def varToFloatOpsCls(x: Var[Float])(implicit pos: SourceContext) = new FloatOpsCls(readVar(x))(pos)

  class FloatOpsCls(val self: Rep[Float])(implicit pos: SourceContext) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded18) = { float_plus(self,unit(__arg1.toFloat)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_plus(self,unit(__arg1)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded20) = { double_plus(self.toDouble,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded21) = { float_plus(self,unit(__arg1.toFloat)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded27) = { float_plus(self,__arg1.toFloat) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_plus(self,__arg1) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded29) = { double_plus(self.toDouble,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded30) = { float_plus(self,__arg1.toFloat) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_plus(self,readVar(__arg1).toFloat) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_plus(self,readVar(__arg1)) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_plus(self.toDouble,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_plus(self,readVar(__arg1).toFloat) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded18) = { float_minus(self,unit(__arg1.toFloat)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_minus(self,unit(__arg1)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded20) = { double_minus(self.toDouble,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded21) = { float_minus(self,unit(__arg1.toFloat)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded27) = { float_minus(self,__arg1.toFloat) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_minus(self,__arg1) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded29) = { double_minus(self.toDouble,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded30) = { float_minus(self,__arg1.toFloat) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_minus(self,readVar(__arg1).toFloat) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_minus(self,readVar(__arg1)) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_minus(self.toDouble,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_minus(self,readVar(__arg1).toFloat) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_times(self,unit(__arg1.toFloat)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded20) = { float_times(self,unit(__arg1)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded21) = { double_times(self.toDouble,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded22) = { float_times(self,unit(__arg1.toFloat)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_times(self,__arg1.toFloat) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded29) = { float_times(self,__arg1) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded30) = { double_times(self.toDouble,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_times(self,__arg1.toFloat) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_times(self,readVar(__arg1).toFloat) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_times(self,readVar(__arg1)) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_times(self.toDouble,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_times(self,readVar(__arg1).toFloat) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded18) = { float_divide(self,unit(__arg1.toFloat)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded19) = { float_divide(self,unit(__arg1)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded20) = { double_divide(self.toDouble,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded21) = { float_divide(self,unit(__arg1.toFloat)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded27) = { float_divide(self,__arg1.toFloat) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded28) = { float_divide(self,__arg1) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded29) = { double_divide(self.toDouble,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded30) = { float_divide(self,__arg1.toFloat) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded37) = { float_divide(self,readVar(__arg1).toFloat) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded38) = { float_divide(self,readVar(__arg1)) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded39) = { double_divide(self.toDouble,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded40) = { float_divide(self,readVar(__arg1).toFloat) }
    def toInt(implicit pos: SourceContext) = float_to_int(self)
    def toDouble(implicit pos: SourceContext) = float_to_double(self)
  }

  def obj_float_parse_float(s: Rep[String])(implicit pos: SourceContext): Rep[Float] = ???
  def float_plus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float]((Adapter.INT(Unwrap(lhs)) + Adapter.INT(Unwrap(rhs))).x)
  def float_minus(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float]((Adapter.INT(Unwrap(lhs)) - Adapter.INT(Unwrap(rhs))).x)
  def float_times(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float]((Adapter.INT(Unwrap(lhs)) * Adapter.INT(Unwrap(rhs))).x)
  def float_divide(lhs: Rep[Float], rhs: Rep[Float])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float]((Adapter.INT(Unwrap(lhs)) / Adapter.INT(Unwrap(rhs))).x)
  def float_abs(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float](Adapter.INT(Unwrap(lhs)).abs().x)

  def float_to_int(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int](Adapter.g.reflect("cast", Unwrap(lhs)))
  def float_to_double(lhs: Rep[Float])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double](Adapter.g.reflect("cast", Unwrap(lhs)))


  /**
   * Int
   */

  object Int {
    def MaxValue(implicit pos: SourceContext) = obj_int_max_value
    def MinValue(implicit pos: SourceContext) = obj_int_min_value
  }

  object Integer {
    def parseInt(s: Rep[String])(implicit __pos: SourceContext)  = obj_integer_parseInt(s)
  }

  implicit def intToIntOps(n: Int): IntOpsCls = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]): IntOpsCls = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int]): IntOpsCls = new IntOpsCls(readVar(n))

  class IntOpsCls(self: Rep[Int]) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { int_plus(self,unit(__arg1)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_plus(self.toFloat,unit(__arg1)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_plus(self.toDouble,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_plus(self.toLong,unit(__arg1)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { int_plus(self,__arg1) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_plus(self.toFloat,__arg1) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_plus(self.toDouble,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_plus(self.toLong,__arg1) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_plus(self,readVar(__arg1)) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_plus(self.toFloat,readVar(__arg1)) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_plus(self.toDouble,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_plus(self.toLong,readVar(__arg1)) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { int_minus(self,unit(__arg1)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_minus(self.toFloat,unit(__arg1)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_minus(self.toDouble,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_minus(self.toLong,unit(__arg1)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { int_minus(self,__arg1) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_minus(self.toFloat,__arg1) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_minus(self.toDouble,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_minus(self.toLong,__arg1) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_minus(self,readVar(__arg1)) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_minus(self.toFloat,readVar(__arg1)) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_minus(self.toDouble,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_minus(self.toLong,readVar(__arg1)) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded16) = { int_times(self,unit(__arg1)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded17) = { float_times(self.toFloat,unit(__arg1)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded18) = { double_times(self.toDouble,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded19) = { long_times(self.toLong,unit(__arg1)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded25) = { int_times(self,__arg1) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded26) = { float_times(self.toFloat,__arg1) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded27) = { double_times(self.toDouble,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded28) = { long_times(self.toLong,__arg1) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_times(self,readVar(__arg1)) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_times(self.toFloat,readVar(__arg1)) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_times(self.toDouble,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_times(self.toLong,readVar(__arg1)) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { int_divide(self,unit(__arg1)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_divide(self.toFloat,unit(__arg1)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_divide(self.toDouble,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_divide(self.toLong,unit(__arg1)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { int_divide(self,__arg1) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_divide(self.toFloat,__arg1) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_divide(self.toDouble,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_divide(self.toLong,__arg1) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded33) = { int_divide(self,readVar(__arg1)) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded34) = { float_divide(self.toFloat,readVar(__arg1)) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded35) = { double_divide(self.toDouble,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded36) = { long_divide(self.toLong,readVar(__arg1)) }
    def %(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_mod(self, __arg1)
    def &(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_binaryand(self, __arg1)
    def |(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_binaryor(self, __arg1)
    def ^(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_binaryxor(self, __arg1)
    def <<(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_leftshift(self, __arg1)
    def >>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_rightshiftarith(self, __arg1)
    def >>>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = int_rightshiftlogical(self, __arg1)
    def unary_~(implicit pos: SourceContext) = int_bitwise_not(self)
    def toLong(implicit pos: SourceContext) = int_to_long(self)
    def toDouble(implicit pos: SourceContext) = int_to_double(self)
    def toFloat(implicit pos: SourceContext) = int_to_float(self)
    def toChar(implicit pos: SourceContext) = int_to_char(self)
  }

  def obj_integer_parseInt(s: Rep[String])(implicit pos: SourceContext): Rep[Int] = ???
  def obj_int_max_value(implicit pos: SourceContext): Rep[Int] = ???
  def obj_int_min_value(implicit pos: SourceContext): Rep[Int] = ???
  def int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int]((Adapter.INT(Unwrap(lhs)) + Adapter.INT(Unwrap(rhs))).x)
  def int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int]((Adapter.INT(Unwrap(lhs)) - Adapter.INT(Unwrap(rhs))).x)
  def int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int]((Adapter.INT(Unwrap(lhs)) * Adapter.INT(Unwrap(rhs))).x)
  def int_divide(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int]((Adapter.INT(Unwrap(lhs)) / Adapter.INT(Unwrap(rhs))).x)
  def int_abs(lhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int](Adapter.INT(Unwrap(lhs)).abs().x)

  def int_mod(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int]((Adapter.INT(Unwrap(lhs)) % Adapter.INT(Unwrap(rhs))).x)
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] = ???
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int](Adapter.g.reflect("&", Unwrap(lhs), Unwrap(rhs)))
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] = ???
  def int_bitwise_not(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Int] = ???
  def int_to_long(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Long] =
    Wrap[Long](Adapter.g.reflect("cast", Unwrap(lhs)))
  def int_to_float(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Float] =
    Wrap[Float](Adapter.g.reflect("cast", Unwrap(lhs)))
  def int_to_char(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Char] =
    Wrap[Char](Adapter.g.reflect("cast", Unwrap(lhs)))
  def int_to_double(lhs: Rep[Int])(implicit pos: SourceContext) : Rep[Double] =
    Wrap[Double](Adapter.g.reflect("cast", Unwrap(lhs)))
  def int_leftshift(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] = ???
  def int_rightshiftarith(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] = ???
  def int_rightshiftlogical(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] = ???

  /**
   * Char
   */

  implicit def charToCharOps(self: Rep[Char]) = new CharOpsCls(self)

  class CharOpsCls(self: Rep[Char]) {
    def toInt(implicit pos: SourceContext) = char_toInt(self)
    def toLong(implicit pos: SourceContext) = char_toLong(self)
  }

  def char_toInt(lhs: Rep[Char])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int](Adapter.g.reflect("cast", Unwrap(lhs)))

  def char_toLong(lhs: Rep[Char])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long](Adapter.g.reflect("cast", Unwrap(lhs)))

  /**
   * Long
   */

  object Long {
    def parseLong(s: Rep[String])(implicit pos: SourceContext) = obj_long_parse_long(s)
    def MaxValue(implicit pos: SourceContext) = obj_long_max_value
    def MinValue(implicit pos: SourceContext) = obj_long_min_value
  }

  implicit def longToLongOps(n: Long): LongOpsCls = new LongOpsCls(unit(n))
  implicit def repLongToLongOps(n: Rep[Long]): LongOpsCls = new LongOpsCls(n)
  implicit def varLongToLongOps(n: Var[Long]): LongOpsCls = new LongOpsCls(readVar(n))

  class LongOpsCls(self: Rep[Long]) {
    // def +(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { long_plus(self,unit(__arg1.toLong)) }
    // def +(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_plus(self.toFloat,unit(__arg1)) }
    // def +(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_plus(self.toDouble,unit(__arg1)) }
    // def +(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_plus(self,unit(__arg1)) }
    // def +(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { long_plus(self,__arg1.toLong) }
    // def +(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_plus(self.toFloat,__arg1) }
    // def +(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_plus(self.toDouble,__arg1) }
    // def +(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_plus(self,__arg1) }
    // def +(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_plus(self,readVar(__arg1).toLong) }
    // def +(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_plus(self.toFloat,readVar(__arg1)) }
    // def +(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_plus(self.toDouble,readVar(__arg1)) }
    // def +(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_plus(self,readVar(__arg1)) }
    // def -(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { long_minus(self,unit(__arg1.toLong)) }
    // def -(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_minus(self.toFloat,unit(__arg1)) }
    // def -(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_minus(self.toDouble,unit(__arg1)) }
    // def -(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_minus(self,unit(__arg1.toLong)) }
    // def -(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { long_minus(self,__arg1) }
    // def -(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_minus(self.toFloat,__arg1) }
    // def -(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_minus(self.toDouble,__arg1) }
    // def -(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_minus(self,__arg1) }
    // def -(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_minus(self,readVar(__arg1).toLong) }
    // def -(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_minus(self.toFloat,readVar(__arg1)) }
    // def -(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_minus(self.toDouble,readVar(__arg1)) }
    // def -(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_minus(self,readVar(__arg1)) }
    // def *(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded16) = { long_times(self,unit(__arg1.toLong)) }
    // def *(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded17) = { float_times(self.toFloat,unit(__arg1)) }
    // def *(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded18) = { double_times(self.toDouble,unit(__arg1)) }
    // def *(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded19) = { long_times(self,unit(__arg1.toLong)) }
    // def *(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded25) = { long_times(self,__arg1.toLong) }
    // def *(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded26) = { float_times(self.toFloat,__arg1) }
    // def *(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded27) = { double_times(self.toDouble,__arg1) }
    // def *(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded28) = { long_times(self,__arg1) }
    // def *(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_times(self,readVar(__arg1).toLong) }
    // def *(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_times(self.toFloat,readVar(__arg1)) }
    // def *(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_times(self.toDouble,readVar(__arg1)) }
    // def *(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_times(self,readVar(__arg1)) }
    // def /(__arg1: Int)(implicit pos: SourceContext,__imp1: Overloaded15) = { long_divide(self,unit(__arg1.toLong)) }
    // def /(__arg1: Float)(implicit pos: SourceContext,__imp1: Overloaded16) = { float_divide(self.toFloat,unit(__arg1)) }
    // def /(__arg1: Double)(implicit pos: SourceContext,__imp1: Overloaded17) = { double_divide(self.toDouble,unit(__arg1)) }
    // def /(__arg1: Long)(implicit pos: SourceContext,__imp1: Overloaded18) = { long_divide(self,unit(__arg1)) }
    // def /(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded24) = { long_divide(self,__arg1.toLong) }
    // def /(__arg1: Rep[Float])(implicit pos: SourceContext,__imp1: Overloaded25) = { float_divide(self.toFloat,__arg1) }
    // def /(__arg1: Rep[Double])(implicit pos: SourceContext,__imp1: Overloaded26) = { double_divide(self.toDouble,__arg1) }
    // def /(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded27) = { long_divide(self,__arg1) }
    // def /(__arg1: Var[Int])(implicit pos: SourceContext,__imp1: Overloaded30) = { long_divide(self,readVar(__arg1).toLong) }
    // def /(__arg1: Var[Float])(implicit pos: SourceContext,__imp1: Overloaded31) = { float_divide(self.toFloat,readVar(__arg1)) }
    // def /(__arg1: Var[Double])(implicit pos: SourceContext,__imp1: Overloaded32) = { double_divide(self.toDouble,readVar(__arg1)) }
    // def /(__arg1: Var[Long])(implicit pos: SourceContext,__imp1: Overloaded33) = { long_divide(self,readVar(__arg1)) }
    def %(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_mod(self, __arg1)
    def &(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_binaryand(self, __arg1)
    def |(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_binaryor(self, __arg1)
    def ^(__arg1: Rep[Long])(implicit pos: SourceContext,__imp1: Overloaded1) = long_binaryxor(self, __arg1)
    def <<(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = long_shiftleft(self, __arg1)
    def >>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = long_shiftright_signed(self, __arg1)
    def >>>(__arg1: Rep[Int])(implicit pos: SourceContext,__imp1: Overloaded1) = long_shiftright_unsigned(self, __arg1)
    def toInt(implicit pos: SourceContext) = long_to_int(self)
    def toDouble(implicit pos: SourceContext) = long_to_double(self)
    def toFloat(implicit pos: SourceContext) = long_to_float(self)
  }

  def long_plus(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long]((Adapter.INT(Unwrap(lhs)) + Adapter.INT(Unwrap(rhs))).x) // XXX type
  def long_minus(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long]((Adapter.INT(Unwrap(lhs)) - Adapter.INT(Unwrap(rhs))).x) // XXX type
  def long_times(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long]((Adapter.INT(Unwrap(lhs)) * Adapter.INT(Unwrap(rhs))).x) // XXX type
  def long_divide(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long]((Adapter.INT(Unwrap(lhs)) / Adapter.INT(Unwrap(rhs))).x) // XXX type
  def long_abs(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long](Adapter.INT(Unwrap(lhs)).abs().x)

  def obj_long_parse_long(s: Rep[String])(implicit pos: SourceContext): Rep[Long] = ???
  def obj_long_max_value(implicit pos: SourceContext): Rep[Long] = ???
  def obj_long_min_value(implicit pos: SourceContext): Rep[Long] = ???
  def long_mod(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] = ???
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long](Adapter.g.reflect("&", Unwrap(lhs), Unwrap(rhs)))
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long](Adapter.g.reflect("|", Unwrap(lhs), Unwrap(rhs)))
  def long_binaryxor(lhs: Rep[Long], rhs: Rep[Long])(implicit pos: SourceContext): Rep[Long] = ???
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long] =
    Wrap[Long](Adapter.g.reflect("<<", Unwrap(lhs), Unwrap(rhs)))
  def long_shiftright_signed(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long] = ???
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Long] = ???
  def long_to_int(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Int] =
    Wrap[Int](Adapter.g.reflect("cast", Unwrap(lhs)))
  def long_to_float(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Float] =
    Wrap[Float](Adapter.g.reflect("cast", Unwrap(lhs)))
  def long_to_double(lhs: Rep[Long])(implicit pos: SourceContext): Rep[Double] =
    Wrap[Double](Adapter.g.reflect("cast", Unwrap(lhs)))
}
