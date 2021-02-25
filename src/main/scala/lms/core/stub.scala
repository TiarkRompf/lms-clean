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

// FIXME(feiw) should the MetaData (typeMap and sourceMap) be part of Adapter?
object Adapter extends FrontEnd {

  override def mkGraphBuilder() = new GraphBuilderOpt

  // We are using these "global" data structures to hold "old" graph metadata in transformation
  var oldTypeMap: mutable.Map[lms.core.Backend.Exp, Manifest[_]] = _
  var oldSourceMap: mutable.Map[lms.core.Backend.Exp, lms.macros.SourceContext] = _
  var oldDefsCache: immutable.Map[lms.core.Backend.Sym, lms.core.Backend.Node] = _

  var typeMap: mutable.Map[lms.core.Backend.Exp, Manifest[_]] = _
  var sourceMap: mutable.Map[lms.core.Backend.Exp, SourceContext] = _
  var funTable: List[(Backend.Exp, Any)] = _
  def resetState = {
    typeMap = new scala.collection.mutable.HashMap[lms.core.Backend.Exp, Manifest[_]]()
    sourceMap = new scala.collection.mutable.HashMap[lms.core.Backend.Exp, SourceContext]()
    funTable = Nil
  }

  def genGraph1(m1: Manifest[_], m2: Manifest[_])(prog: Exp => Exp) = {
    genGraphCommon(m1, m2)(g.reify(prog))
  }
  def genGraphCommon(m1: Manifest[_], m2: Manifest[_])(prog: => Block) = {
    resetState
    var g: Graph = time("staging") { program(prog) }
    g
  }

  def emitCommon1(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream,
                  verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)
                 (m1: Manifest[_], m2: Manifest[_])(prog: Exp => Exp) =
    emitCommon(name, cg, stream, verbose, alt, eff)(m1, m2)(g.reify(prog))

  def emitCommon2(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream,
                  verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)
                 (m1: Manifest[_], m2: Manifest[_])(prog: (Exp, Exp) => Exp) =
    emitCommon(name, cg, stream, verbose, alt, eff)(m1, m2)(g.reify(prog))

  def emitCommon(name: String, cg: ExtendedCodeGen, stream: java.io.PrintStream,
                 verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)
                (m1: Manifest[_], m2: Manifest[_])(prog: => Block) = {
    resetState

    var g: Graph = time("staging") { program(prog) }

    def extra() = if (verbose) utils.captureOut {
      println("// Raw:")
      g.nodes.foreach(println)

      println("// Generic Codegen:")
      (new GenericCodeGen)(g)

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
}


/**
 * BaseTypeless trait
 */
object BaseTypeLess {

  class TOP(val x: Backend.Exp, val useOldMetadata: Boolean = false) {
    def withSource(pos: SourceContext): this.type = { Adapter.sourceMap(x) = pos; this }
    def withType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    def withSrcType(pos: SourceContext, m: Manifest[_]): this.type = withSource(pos).withType(m)

    // FIXME(feiw): Q1: should these code be put here
    // FIXME(feiw): Q2: should these code be `val` instead of `def`
    def p: SourceContext = if (useOldMetadata) Adapter.oldSourceMap(x) else Adapter.sourceMap(x)
    def t: Manifest[_] = if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)
    def gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache

    def equals(that: this.type) = x == that.x
    def castTo(mY: Manifest[_])(implicit pos: SourceContext) = TOP(Adapter.g.reflect("cast", x, Backend.Const(mY)), mY)
  }

  def TOP(x: Backend.Exp, m: Manifest[_])(implicit __pos: SourceContext): TOP =
    (new TOP(x)).withSrcType(__pos, m)


  class UNIT(override val x: Backend.Exp) extends TOP(x) {
    Adapter.typeMap(x) = manifest[Unit]
  }
  def UNIT(x: Backend.Exp)(implicit __pos: SourceContext): UNIT = (new UNIT(x)).withSource(__pos)
  def UNIT(x: TOP): UNIT = new UNIT(x.x)


  class BOOL(override val x: Backend.Exp) extends TOP(x) {
    Adapter.typeMap(x) = manifest[Boolean]
    def unary_!(implicit __pos: SourceContext) = BOOL(Adapter.g.reflect("!",x))
  }
  def BOOL(x: Backend.Exp)(implicit __pos: SourceContext): BOOL = (new BOOL(x)).withSource(__pos)

  // NOTE(feiw) hope to check the manifest but it is not so easy since manifest do not have to be the same
  def NOT_EQUAL(a: TOP, b: TOP)(implicit pos: SourceContext): BOOL =
    BOOL(Adapter.g.reflect("!=", a.x, b.x))
  def EQUAL(a: TOP, b: TOP)(implicit pos: SourceContext): BOOL =
    BOOL(Adapter.g.reflect("==", a.x, b.x))


  class VAR(override val x: Backend.Exp) extends TOP(x) {
    def et: Manifest[_] = Adapter.typeMap(x)

    // FIXME(feiw) the return type (TOP) might be too generic/relaxed
    def apply(implicit __pos: SourceContext): TOP =
      TOP(Adapter.g.reflectRead("var_get",x)(x), et)
    def update(y: TOP)(implicit __pos: SourceContext): UNIT = {
      assert(et == y.t)
      UNIT(Adapter.g.reflectWrite("var_set", x, y.x)(x))
    }
  }
  def VAR(x: TOP)(implicit __pos: SourceContext): VAR =
    (new VAR(Adapter.g.reflectMutable("var_new", x.x))).withSrcType(__pos, x.t)

  def IF(c: BOOL)(a: => TOP)(b: => TOP)(implicit __pos: SourceContext): TOP = {
    val aBlock = Adapter.g.reifyHere(a.x)
    val bBlock = Adapter.g.reifyHere(b.x)
    // compute effect (aBlock || bBlock)
    val pure = aBlock.isPure && bBlock.isPure
    val aManifest = Adapter.typeMap(aBlock.res)
    val bManifest = Adapter.typeMap(bBlock.res)
    // NOTE(feiw) hope to check the equality of the `aManifest` and `bManifest` but
    // that might not consider subtypes
    if (pure)
      TOP(Adapter.g.reflect("?",c.x,aBlock,bBlock), aManifest)
    else
      TOP(Adapter.g.reflectEffectSummaryHere("?",c.x,aBlock,bBlock)(Adapter.g.mergeEffKeys(aBlock, bBlock)), aManifest)
  }

  def WHILE(c: => BOOL)(b: => Unit)(implicit __pos: SourceContext): UNIT = {
    val cBlock = Adapter.g.reify(c.x)
    val bBlock = Adapter.g.reify({ b; Backend.Const(()) } )
    // compute effect (cBlock bBlock)* cBlock
    UNIT(Adapter.g.reflectEffectSummary("W", cBlock,
      bBlock)(Adapter.g.mergeEffKeys(cBlock, bBlock)))
  }

  def PRINTF(f: String, xs: TOP*)(implicit pos: SourceContext): UNIT =
    UNIT(Adapter.g.reflectWrite("printf", Backend.Const(f)::xs.map(_.x).toList:_*)(Adapter.CTRL))
}

/**
 * Base trait:
 * 1. definition of iconic Rep[T] and Var[T]
 * 2. Wrap[T] and Unwrap[T] using Adapter.typeMap to track Rep[T] and metalevel
 * 3. handing of recursive function definition via Adapter.funTable
 * 4. control flows such as conditional, loop (with @virtualize macro)
 * 5. other basics: misc, print, unchecked, timer, et al.
 * 6. other extentions are at later part of this file or can be extended by DSL writer
 *    including UtilOps, RangeOps, Equal, OrderingOps, PrimitiveOps, LiftPrimitiveOps, et al.
 */
trait Base extends EmbeddedControls with OverloadHack with lms.util.ClosureCompare {
  type Rep[+T] = Exp[T];

  abstract class Exp[+T]
  abstract class Var[T]
  abstract class Def[+T]
  // abstract class Block[T]

  case class Const[T](x: T) extends Exp[T]
  case class Sym[T](x: Int) extends Exp[T]
  case class EffectView[A:Manifest](x: Rep[A], base: Rep[A]) extends Exp[A]

  case class Wrap[+A:Manifest](x: lms.core.Backend.Exp) extends Exp[A] {
    Adapter.typeMap.getOrElseUpdate(x, manifest[A])
  }
  def Wrap[A:Manifest](x: lms.core.Backend.Exp): Exp[A] = {
    if (manifest[A] == manifest[Unit]) Const(()).asInstanceOf[Exp[A]]
    else new Wrap[A](x)
  }
  def Unwrap(x: Exp[Any]) = x match {
    case Wrap(x) => x
    case Const(x) => Backend.Const(x)
    case EffectView(Wrap(x), _) => x // TODO: fix!
  }

  case class WrapV[A:Manifest](x: lms.core.Backend.Exp) extends Var[A] {
    Adapter.typeMap(x) = manifest[A] // could include Var type in manifest
  }
  def UnwrapV[T](x: Var[T]) = x match { case WrapV(x) => x }

  def convertToExp(x: Any) = x match {
    case e: Exp[Any] => Unwrap(e)
    case c => Backend.Const(c)
  }

  implicit def unit[T:Manifest](x: T): Rep[T] = Wrap[T](Backend.Const(x))

  // Functions
  def unwrapFun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Backend.Exp => Backend.Exp =
    (x1: Backend.Exp) => Unwrap(f(Wrap[A](x1)))
  def unwrapFun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A],Rep[B]) => Rep[C]) =
    (x1: Backend.Exp, x2: Backend.Exp) => Unwrap(f(Wrap[A](x1), Wrap[B](x2)))

  def fun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A => B] =
    Wrap[A=>B](__fun(f, 1, xn => Unwrap(f(Wrap[A](xn(0))))))

  def doLambda[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A => B] = fun(f)
  implicit class FunOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = {
      Wrap[B](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x))()())
    }
  }

  def fun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] =
    Wrap[(A,B)=>C](__fun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] = fun(f)
  implicit class FunOps2[A:Manifest,B:Manifest,C:Manifest](f: Rep[(A,B) => C]) {
    def apply(x: Rep[A], y: Rep[B]): Rep[C] =
      Wrap[C](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] =
    Wrap[(A,B,C)=>D](__fun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] = fun(f)
  implicit class FunOps3[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: Rep[(A,B,C) => D]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C]): Rep[D] =
      Wrap[D](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] =
    Wrap[(A,B,C,D)=>E](__fun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] = fun(f)
  implicit class FunOps4[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: Rep[(A,B,C,D) => E]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D]): Rep[E] =
      Wrap[E](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => Rep[F]): Rep[(A, B, C, D, E) => F] =
    Wrap[(A,B,C,D,E)=>F](__fun(f, 5, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => Rep[F]): Rep[(A, B, C, D, E) => F] = fun(f)
  implicit class FunOps5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](f: Rep[(A,B,C,D,E) => F]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D], k: Rep[E]): Rep[F] =
      Wrap[F](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w),Unwrap(k))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F]) => Rep[G]): Rep[(A, B, C, D, E, F) => G] =
    Wrap[(A,B,C,D,E,F)=>G](__fun(f, 6, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F]) => Rep[G]): Rep[(A, B, C, D, E, F) => G] = fun(f)
  implicit class FunOps6[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest](f: Rep[(A,B,C,D,E,F) => G]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D], k: Rep[E], l: Rep[F]): Rep[G] =
      Wrap[G](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w),Unwrap(k),Unwrap(l))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G]) => Rep[H]):
    Rep[(A, B, C, D, E, F, G) => H] =
    Wrap[(A,B,C,D,E,F,G)=>H](__fun(f, 7, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G]) => Rep[H]):
    Rep[(A, B, C, D, E, F, G) => H] = fun(f)
  implicit class FunOps7[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest](f: Rep[(A,B,C,D,E,F,G) => H]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D], k: Rep[E], l: Rep[F], m: Rep[G]): Rep[H] =
      Wrap[H](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w),Unwrap(k),Unwrap(l),Unwrap(m))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H]) => Rep[I]):
    Rep[(A, B, C, D, E, F, G, H) => I] =
    Wrap[(A,B,C,D,E,F,G,H)=>I](__fun(f, 8, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H]) => Rep[I]):
    Rep[(A, B, C, D, E, F, G, H) => I] = fun(f)
  implicit class FunOps8[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest](f: Rep[(A,B,C,D,E,F,G,H) => I]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D], k: Rep[E], l: Rep[F], m: Rep[G], n: Rep[H]): Rep[I] =
      Wrap[I](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w),Unwrap(k),Unwrap(l),Unwrap(m),Unwrap(n))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest]
      (f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I]) => Rep[J]) =
    Wrap[(A,B,C,D,E,F,G,H,I)=>J](__fun(f, 8,
      xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)), Wrap[I](xn(8))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest]
      (f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I]) => Rep[J]) = fun(f)
  implicit class FunOps9[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest](f: Rep[(A,B,C,D,E,F,G,H,I) => J]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D], k: Rep[E], l: Rep[F], m: Rep[G], n: Rep[H], o: Rep[I]): Rep[J] =
      Wrap[J](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w),Unwrap(k),Unwrap(l),Unwrap(m),Unwrap(n),Unwrap(o))()())
  }

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest,K:Manifest]
      (f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I], Rep[J]) => Rep[K]) =
    Wrap[(A,B,C,D,E,F,G,H,I,J)=>K](__fun(f, 9,
      xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)), Wrap[I](xn(8)), Wrap[J](xn(9))))))

  def doLambda[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest,K:Manifest]
      (f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I], Rep[J]) => Rep[K]) = fun(f)
  implicit class FunOps10[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest,K:Manifest](f: Rep[(A,B,C,D,E,F,G,H,I,J) => K]) {
    def apply(x: Rep[A], y: Rep[B], z: Rep[C], w: Rep[D], k: Rep[E], l: Rep[F], m: Rep[G], n: Rep[H], o: Rep[I], p: Rep[J]): Rep[K] =
      Wrap[K](Adapter.g.reflectEffect("@",Unwrap(f),Unwrap(x),Unwrap(y),Unwrap(z),Unwrap(w),Unwrap(k),Unwrap(l),Unwrap(m),Unwrap(n),Unwrap(o),Unwrap(p))()())
  }

  def __fun[T:Manifest](f: AnyRef, arity: Int, gf: List[Backend.Exp] => Backend.Exp, captures: Backend.Exp*): Backend.Exp = {
    val can = canonicalize(f)
    Adapter.funTable.find(_._2 == can) match {
      case Some((funSym, _)) =>
        funSym
      case _ =>
        // Step 1. set up "λforward" node with 2 new fresh Syms
        val fn = Backend.Sym(Adapter.g.fresh)
        val fn1 = Backend.Sym(Adapter.g.fresh)
        Adapter.g.reflect(fn, "λforward", fn1, Backend.Const(arity))()

        // Step 2. register (fn, can) in funTable, so that recursive calls
        //    will find fn as the function Sym. Reify the block.
        // Note: it might seem strange why/how recursive calls re-enter this __fun() function.
        //    The reason is that in user code, recursive functions have to be written as
        //    lazy val f = fun{...} or def f = fun{...}, in which case the recursive calls
        //    will re-enter the `fun` call.
        Adapter.funTable = (fn, can)::Adapter.funTable
        val block = Adapter.g.reify(arity, gf)

        // Step 3. build the "λ" node with fn1 as the function name
        //    fix the funTable such that it pairs (fn1, can) for non-recursive uses.
        val res = Adapter.g.reflect(fn1,"λ",(block+:captures):_*)(hardSummary(fn))
        Adapter.funTable = Adapter.funTable.map {
          case (fn2, can2) => if (can == can2) (fn1, can) else (fn2, can2)
        }
        res
    }
  }

  // Top-Level Functions
  // XXX: is this data structure needed? should it move elsewhere?
  // could we use funTable instead?
  val topLevelFunctions = new scala.collection.mutable.HashMap[AnyRef,Backend.Sym]()
  def __topFun(f: AnyRef, arity: Int, gf: List[Backend.Exp] => Backend.Exp, decorator: String = ""): Backend.Exp = {
    val can = canonicalize(f)
    Adapter.funTable.find(_._2 == can) match {
      case Some((funSym, _)) => funSym
      case _ =>
        val fn = Backend.Sym(Adapter.g.fresh)
        Adapter.funTable = (fn, can)::Adapter.funTable
        val block = Adapter.g.reify(arity, gf)
        val res = Adapter.g.reflect(fn, "λ", block, Backend.Const(0), Backend.Const(decorator))()
        topLevelFunctions.getOrElseUpdate(can, fn)
        fn
    }
  }
  def topFun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A => B] =
    Wrap[A=>B](__topFun(f, 1, xn => Unwrap(f(Wrap[A](xn(0))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] =
    Wrap[(A,B)=>C](__topFun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] =
    Wrap[(A,B,C)=>D](__topFun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] =
    Wrap[(A,B,C,D)=>E](__topFun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3))))))

  def topFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => Rep[F]): Rep[(A, B, C, D, E) => F] =
    Wrap[(A,B,C,D,E)=>F](__topFun(f, 5, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4))))))

  // IfThenElse
  def __ifThenElse[T:Manifest](c: Rep[Boolean], a: => Rep[T], b: => Rep[T])(implicit pos: SourceContext): Rep[T] = c match {
    case Wrap(Backend.Const(true))  => a
    case Wrap(Backend.Const(false)) => b
    case _ =>
      Wrap[T](Adapter.IF(Adapter.BOOL(Unwrap(c)))
                     (Adapter.INT(Unwrap(a)))
                     (Adapter.INT(Unwrap(b))).x)
  }

  def switch[T:Manifest](x: Rep[T], default: Option[() => Unit] = None)(cases: (Seq[T], Rep[T] => Unit)*): Unit = {
    var isPure = true
    val bCases: Seq[Backend.Def]  = cases.flatMap {
      case (Seq(), _)  => ???
      case (s, f) =>
        val block = Adapter.g.reifyHere({f(if (s.length == 1) unit(s.head) else x); Backend.Const(())})
        isPure &&= block.isPure
        Seq(Backend.Const(s.map(Backend.Const(_))), block)
    }

    val bDefault = default.map { f =>
      val block = Adapter.g.reifyHere({ f(); Backend.Const(()) })
      isPure &&= block.isPure
      block
    }

    if (isPure)
      Adapter.g.reflect("switch", Unwrap(x) +: bDefault.getOrElse(Backend.Const(())) +: bCases: _*)
    else
      Adapter.g.reflectEffectSummaryHere("switch", Unwrap(x) +: bDefault.getOrElse(Backend.Const(())) +: bCases:_*)((Set[Backend.Exp](), Set[Backend.Exp]()))
  }

  // While
  def __whileDo(c: => Rep[Boolean], b: => Rep[Unit]): Rep[Unit] = {
      Adapter.WHILE(Adapter.BOOL(Unwrap(c)))(b)
  }
  def __whileDoInternal(c: => Rep[Boolean], b: => Rep[Unit]): Rep[Unit] = {
      Adapter.WHILE(Adapter.BOOL(Unwrap(c)))(b)
  }

  // Variables
  implicit def readVar[T:Manifest](x: Var[T]): Rep[T] = Wrap[T](Adapter.g.reflectRead("var_get", UnwrapV(x))(UnwrapV(x)))
  def var_new[T:Manifest](x: Rep[T]): Var[T] = WrapV[T](Adapter.g.reflectMutable("var_new", Unwrap(x)))
  def __assign[T:Manifest](lhs: Var[T], rhs: Rep[T]): Unit = Wrap[Unit](Adapter.g.reflectWrite("var_set", UnwrapV(lhs), Unwrap(rhs))(UnwrapV(lhs)))
  def __assign[T:Manifest](lhs: Var[T], rhs: Var[T]): Unit = __assign(lhs,readVar(rhs))
  def __assign[T:Manifest](lhs: Var[T], rhs: T): Unit = __assign(lhs,unit(rhs)) // shouldn't unit lift T to Rep[T] implicitly?
  def __newVar[T:Manifest](init: T)(implicit pos: SourceContext) = var_new(unit(init))
  def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Manifest[T], pos: SourceContext) = var_new(init)
  def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Manifest[T], pos: SourceContext) = var_new(init)

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

  // MiscOps
  def exit(res: Rep[Int]): Unit = Adapter.g.reflectWrite("exit", Unwrap(res))(Adapter.CTRL)
  def println(x: Rep[Any]): Unit =
    Adapter.g.reflectWrite("P",Unwrap(x))(Adapter.CTRL)
  def printf(f: String, x: Rep[Any]*): Unit = {
    // for (a <- f.split('%'))
    //   System.out.println(s"a:$a")
    // f.split("%").map(_.charAt(0)) zip(x.map(a => Adapter.typeMap.getOrElse(Unwrap(a), manifest[Unknown]))) foreach {
    //   case ('d', m) => assert(m == manifest[Int] || m == manifest[Long])
    //   case ('f', m) => assert(m == manifest[Float] || m == manifest[Double])
    //   case ('s', m) => assert(m == manifest[String])
    //   case _ => () // FIXME(feiw) 1. add other types 2. add other format support %4X
    // }
    Adapter.g.reflectWrite("printf",Backend.Const(f)::x.map(Unwrap).toList:_*)(Adapter.CTRL)
  }
  def staticData[T:Manifest](x: T): Rep[T] =
    Wrap[T](Adapter.g.reflect("staticData", Backend.Const(x)))

  // BooleanOps
  implicit def bool2boolOps(lhs: Boolean) = new BoolOps(lhs)
  implicit def var2boolOps(lhs: Var[Boolean]) = new BoolOps(lhs)
  implicit class BoolOps(lhs: Rep[Boolean]) {
    def unary_!(implicit pos: SourceContext): Rep[Boolean] = Wrap[Boolean](Adapter.g.reflect("!", Unwrap(lhs)))
    def &&(rhs: =>Rep[Boolean])(implicit pos: SourceContext) =
    __ifThenElse(lhs, rhs, unit(false))
    def ||(rhs: =>Rep[Boolean])(implicit pos: SourceContext) =
      __ifThenElse(lhs, unit(true), rhs)
    def ^(rhs: Rep[Boolean]) = Wrap[Boolean](Adapter.g.reflect("!=", Unwrap(lhs), Unwrap(rhs)))
  }

  // TimingOps
  def timeGeneratedCode[A: Manifest](f: => Rep[A], msg: Rep[String] = unit("")): Rep[A] = {
    val ff = Adapter.g.reify(Unwrap(f))
    val summary = Adapter.g.getEffKeys(ff)
    Wrap[A](Adapter.g.reflectEffectSummary("timeGenerated", Unwrap(msg), ff)(summary))
  }
  def timestamp: Rep[Long] = Wrap[Long](Adapter.g.reflectWrite("timestamp")(Adapter.CTRL))

  // case class GenerateComment(l: String) extends Def[Unit]
  // case class Comment[A:Manifest](l: String, verbose: Boolean, b: Block[A]) extends Def[A]
  def generate_comment(l: String): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("generate-comment", Backend.Const(l))(Adapter.CTRL))
  }
  def withComment[T](s: String)(clo: => T): T = {
    generate_comment(s"begin $s")
    val kernel = clo
    generate_comment(s"end $s")
    kernel
  }
  def comment[A:Manifest](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A] = {
    val g = Adapter.g
    val bb = g.reify(Unwrap(b))
    if (bb.isPure)
      Wrap[A](g.reflect("comment",Backend.Const(l),Backend.Const(verbose),bb))
    else {
      val summary = Adapter.g.getEffKeys(bb)
      Wrap[A](g.reflectEffectSummary("comment",Backend.Const(l),Backend.Const(verbose),bb)(summary))
    }
  }

  // StringOps
  implicit class StringOps(lhs: Rep[String]) {
    def charAt(i: Rep[Int]): Rep[Char] = Wrap[Char](Adapter.g.reflect("String.charAt", Unwrap(lhs), Unwrap(i))) // XXX: may fail! effect?
    def apply(i: Rep[Int]): Rep[Char] = charAt(i)
    def length: Rep[Int] = Wrap[Int](Adapter.g.reflect("String.length", Unwrap(lhs)))
    def substring(idx: Rep[Int], end: Rep[Int]): Rep[String] = Wrap[String](Adapter.g.reflect("String.slice", Unwrap(lhs), Unwrap(idx), Unwrap(end))) // FIXME: View
    def toInt: Rep[Int] = Wrap[Int](Adapter.g.reflect("String.toInt", Unwrap(lhs))) // XXX: may fail!
    def toDouble: Rep[Double] = Wrap[Double](Adapter.g.reflect("String.toDouble", Unwrap(lhs))) // XXX: may fail!
  }

  // UncheckedOps
  private def uncheckedHelp(xs: Seq[Any]) = {
    val str = xs map {
      case s: String => s
      case e: Exp[Any] => "[ ]"
      case e: Var[Any] => "[ ]"
      case e => e.toString
    } mkString("")
    var effs = Set[Backend.Exp]()
    val args = xs collect {
      case e: Exp[Any] =>
        val res = Unwrap(e)
        if (res.isInstanceOf[Backend.Sym])
          effs += res
        res
      case v: Var[Any] =>
        val res = UnwrapV(v)
        effs += res
        res
    }
    (str, args, effs)
  }

  // TODO:
  // - More than one
  // - filter non mutable or detect arrays?
  def unchecked[T:Manifest](xs: Any*): Rep[T] = {
    val (strings, args, effs) = uncheckedHelp(xs)
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args:_*)()((effs + Adapter.CTRL).toSeq:_*))
  }
  def uncheckedWrite[T:Manifest](xs: Any*)(x: Any): Rep[T] = {
    val (strings, args, effs) = uncheckedHelp(xs)
    x match {
      case x: Var[_] => assert(effs.size == 1 && effs(UnwrapV(x)), "additional effects detected in uncheckedWrite, please use unchecked")
      case x: Exp[_] => assert(effs.size == 1 && effs(Unwrap(x)), "additional effects detected in uncheckedWrite, please use unchecked")
    }
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args:_*)()((effs + Adapter.CTRL).toSeq:_*))
  }
  def uncheckedRead[T:Manifest](xs: Any*)(x: Any): Rep[T] = {
    val (strings, args, effs) = uncheckedHelp(xs)
    x match {
      case x: Var[_] => assert(effs.size == 1 && effs(UnwrapV(x)), "additional effects detected in uncheckedRead, please use unchecked")
      case x: Exp[_] => assert(effs.size == 1 && effs(Unwrap(x)), "additional effects detected in uncheckedRead, please use unchecked")
    }
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args:_*)(effs.toSeq:_*)(Adapter.CTRL))
  }
  def uncheckedPure[T:Manifest](xs: Any*): Rep[T] = {
    val (strings, args, _) = uncheckedHelp(xs)
    // assert(effs.isEmpty, "variable detected in uncheckedPure, please use unchecked*Write, unchecked*Read, or unchecked")
    Wrap[T](Adapter.g.reflect("unchecked" + strings, args:_*))
  }

  def uncheckedPureRead[T:Manifest](xs: Any*)(x: Any): Rep[T] = {
    val (strings, args, effs) = uncheckedHelp(xs)
    x match {
      case x: Var[_] => assert(effs.size == 1 && effs(UnwrapV(x)), "additional effects detected in uncheckedRead, please use unchecked")
      case x: Exp[_] => assert(effs.size == 1 && effs(Unwrap(x)), "additional effects detected in uncheckedRead, please use unchecked")
    }
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args:_*)(effs.toSeq:_*)())
  }
  def uncheckedPureWrite[T:Manifest](xs: Any*)(x: Any): Rep[T] = {
    val (strings, args, effs) = uncheckedHelp(xs)
    x match {
      case x: Var[_] => assert(effs.size == 1 && effs(UnwrapV(x)), "additional effects detected in uncheckedPureWrite, please use unchecked")
      case x: Exp[_] => assert(effs.size == 1 && effs(Unwrap(x)), "additional effects detected in uncheckedPureWrite, please use unchecked")
    }
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args:_*)()(effs.toSeq:_*))
  }

  // full user control of the effects
  def uncheckedEffect[T:Manifest](xs: Any*)(rkeys: Rep[_]*)(wkeys: Rep[_]*): Rep[T] = {
    val (strings, args, effs) = uncheckedHelp(xs)
    val writeKeys = Adapter.CTRL +: wkeys.map(Unwrap)
    Wrap[T](Adapter.g.reflectEffect("unchecked" + strings, args: _*)(rkeys.map(Unwrap): _*)(writeKeys: _*))
  }

  // Def[T]: FIXME(feiw) Do we still need them?
  implicit def toAtom[T:Manifest](x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflect(p.productPrefix, xs:_*))
  }
  def reflectEffect[T:Manifest](x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectWrite(p.productPrefix, xs:_*)(Adapter.CTRL))
  }
  def reflectMutable[T:Manifest](x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectMutable(p.productPrefix, xs:_*))
  }
  def reflectWrite[T:Manifest](w: Rep[Any])(x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectWrite(p.productPrefix, xs:_*)(Unwrap(w)))
  }
  def reflectFree[T:Manifest](w: Rep[Any])(x: Def[T]): Exp[T] = {
    val p = x.asInstanceOf[Product]
    val xs = p.productIterator.map(convertToExp).toSeq
    Wrap[T](Adapter.g.reflectFree(p.productPrefix, xs:_*)(Unwrap(w)))
  }

  // shift/reset
  def shift1[A:Manifest, B:Manifest](f: Rep[A => B] => Rep[B]): Rep[A] = { // XXX is the type signature correct?
    val bBlock = Adapter.g.reify(x => Unwrap(f(Wrap[A => B](x))))
    Wrap[A](Adapter.g.reflectWrite("shift1", bBlock)(Adapter.CPS))
  }
  def reset1[A:Manifest](f: => Rep[A]): Rep[A] = { // XXX is the type signature correct?
    val rBlock = Adapter.g.reify(Unwrap(f))
    Wrap[A](Adapter.g.reflectEffect("reset1", rBlock)()())
  }

  // ImplicitOps
  // TR: I'm concerned about this because it is overly general -- it doesn't appear to be used anyways?
  // def implicit_convert[A:Manifest,B:Manifest](a: Rep[A]): Rep[B] = Wrap[B](Adapter.g.reflect("convert", Unwrap(a)))
}

trait UtilOps extends Base {
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

  case class ObjHashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext) extends Def[Long] { def m = typ[T] }
  case class StrSubHashCode(o: Rep[String], len: Rep[Int])(implicit pos: SourceContext) extends Def[Long]
  def infix_HashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext) = ObjHashCode(o)
  def infix_HashCodeS(o: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext) = StrSubHashCode(o,len)
}


object RangeTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._

  class RANGE(override val x: Backend.Exp) extends TOP(x) {
    this.withType(manifest[Range])

    def foreach(f: INT => UNIT)(implicit __pos: SourceContext) = x match {
      case Adapter.g.Def("range_until", List(x0: Backend.Exp, x1: Backend.Exp)) =>
        val i = VAR(INT(x0))
        WHILE(NOT_EQUAL(INT(i(__pos)), INT(x1))) {
          f(INT(i(__pos)))
          i.update(INT(i(__pos)) + 1); ()
        }
    }
  }
  def RANGE(x: Backend.Exp)(implicit __pos: SourceContext): RANGE = (new RANGE(x)).withSource(__pos)

  def RANGE_UNTIL(start: INT, end: INT)(implicit __pos: SourceContext) =
    RANGE(Adapter.g.reflect("range_until", start.x, end.x))

}

trait RangeOps extends Equal with OrderingOps {

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
    def foreach(f: Rep[Int] => Rep[Unit])(implicit __pos: SourceContext): Rep[Unit] = Unwrap(lhs) match {

      // XXX TODO: it would be good to do this as lowering/codegen
      // (as done previously in LMS), but for now just construct
      // a while loop directly
      case Adapter.g.Def("range_until", List(x0:Backend.Exp, x1:Backend.Exp)) =>

        // val b = Adapter.g.reify(i => Unwrap(f(Wrap(i))))
        // val f1 = Adapter.g.reflect("λ",b)
        // Wrap(Adapter.g.reflect("range_foreach", x0, x1, b))

        val i = var_new(Wrap[Int](x0))
        __whileDo(notequals(readVar(i), Wrap[Int](x1)), {
          f(readVar(i))
          i += 1
        })

      case Adapter.g.Def("range_until_step", List(x0:Backend.Exp, x1:Backend.Exp, x2:Backend.Exp)) =>
        val i = var_new(Wrap[Int](x0))
        __whileDo(ordering_lt(readVar(i), Wrap[Int](x1)), {
          f(readVar(i))
          i += Wrap[Int](x2)
        })

      case n => ???
    }
  }

  implicit class SteppedRangeConstrOps(lhs: Rep[Int]) {
    def until(y: Rep[Int], by: Rep[Int]): Rep[Range] = range_until_step(lhs, y, by)
  }
  def range_until_step(start: Rep[Int], end: Rep[Int], by: Rep[Int]): Rep[Range] = {
    Wrap[Range](Adapter.g.reflect("range_until_step", Unwrap(start), Unwrap(end), Unwrap(by)))
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


object PrimitiveTypeLess {
  import BaseTypeLess._

  def NUM_ONE(m: Manifest[_])(implicit __pos: SourceContext): NUM = m match {
    case man if (man == manifest[Int]) => (new NUM(Const(1))).withSrcType(__pos, m)
    case man if (man == manifest[Float]) => (new NUM(Const(1.toFloat))).withSrcType(__pos, m)
    case man if (man == manifest[Double]) => (new NUM(Const(1.toDouble))).withSrcType(__pos, m)
  }

  def NUM_ZERO(m: Manifest[_])(implicit __pos: SourceContext): NUM = m match {
    case man if (man == manifest[Int]) => (new NUM(Const(0))).withSrcType(__pos, m)
    case man if (man == manifest[Float]) => (new NUM(Const(0.toFloat))).withSrcType(__pos, m)
    case man if (man == manifest[Double]) => (new NUM(Const(0.toDouble))).withSrcType(__pos, m)
  }

  class NUM(override val x: Backend.Exp) extends TOP(x) {
    def +(y: NUM)(implicit pos: SourceContext): NUM = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}")
      NUM(Adapter.g.reflect("+", x, y.x), t)
    }
    def -(y: NUM)(implicit pos: SourceContext): NUM = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}")
      NUM(Adapter.g.reflect("-", x, y.x), t)
    }
    def *(y: NUM)(implicit pos: SourceContext): NUM = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}:: ${x} ${y}")
      NUM(Adapter.g.reflect("*", x, y.x), t)
    }
    def /(y: NUM)(implicit pos: SourceContext): NUM = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}")
      NUM(Adapter.g.reflect("/", x, y.x),t)
    }
    def %(y: NUM)(implicit pos: SourceContext): NUM = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}")
      NUM(Adapter.g.reflect("%", x, y.x), t)
    }

    def <(y: NUM)(implicit pos: SourceContext): BOOL = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}")
      BOOL(Adapter.g.reflect("<", x, y.x))
    }

    def >(y: NUM)(implicit pos: SourceContext): BOOL = {
      assert(t == y.t, s"t ${t} is not the same as y.t ${y.t}")
      BOOL(Adapter.g.reflect(">", x, y.x))
    }

    def tanh()(implicit pos: SourceContext): NUM = {
      NUM(Adapter.g.reflect("tanh", x), t)
    }

    def max(y: NUM)(implicit pos: SourceContext): NUM = {
      NUM(Adapter.g.reflect("max", x, y.x), t)
    }

    def relu_grad(m: Manifest[_])(implicit pos: SourceContext): NUM = {
      NUM(IF (this > NUM_ZERO(m)) {
        NUM_ONE(m)
      } {
        NUM_ZERO(m)
      })
    }
  }
  def NUM(x: Backend.Exp, m: Manifest[_])(implicit __pos: SourceContext): NUM =
    (new NUM(x)).withSrcType(__pos, m).asInstanceOf[NUM]
  def NUM(x: TOP): NUM = new NUM(x.x)


  class INT(override val x: Backend.Exp) extends NUM(x) {
    this.withType(manifest[Int])
  }
  def INT(x: Backend.Exp)(implicit __pos: SourceContext): INT = (new INT(x)).withSource(__pos)
  implicit def INT(i: Int)(implicit __pos: SourceContext): INT = (new INT(Backend.Const(i))).withSource(__pos)
  def INT(x: TOP): INT = (new INT(x.x))

  class FLOAT(override val x: Backend.Exp) extends NUM(x) {
    this.withType(manifest[Float])
  }
  def FLOAT(x: Backend.Exp)(implicit __pos: SourceContext): FLOAT = (new FLOAT(x)).withSource(__pos)
  implicit def FLOAT(i: Float)(implicit __pos: SourceContext): FLOAT = (new FLOAT(Backend.Const(i))).withSource(__pos)

  def FLOAT(x: TOP): FLOAT = (new FLOAT(x.x))
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
   * Math API
   */
  object Math {
    def abs(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.abs[Double](x)
    def abs(x: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded43): Rep[Float] = Arithmetic.abs[Float](x)
    def abs(x: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded44): Rep[Long] = Arithmetic.abs[Long](x)
    def abs(x: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded45): Rep[Int] = Arithmetic.abs[Int](x)

    def tanh(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.tanh[Double](x)
    def sin(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.sin[Double](x)
    def cos(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.cos[Double](x)
    def exp(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.exp[Double](x)
    def log(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.log[Double](x)
    def sqrt(x: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42): Rep[Double] = Arithmetic.sqrt[Double](x)
  }

  // private Arithmetic object for code reuse
  private[this] object Arithmetic {
    def plus[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("+", Unwrap(lhs), Unwrap(rhs)))
    def minus[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("-", Unwrap(lhs), Unwrap(rhs)))
    def times[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("*", Unwrap(lhs), Unwrap(rhs)))
    def divide[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("/", Unwrap(lhs), Unwrap(rhs)))
    def mod[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("%", Unwrap(lhs), Unwrap(rhs)))
    def unary_-[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("-", Unwrap(lhs)))
    def abs[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("abs", Unwrap(lhs)))
    def sin[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("sin", Unwrap(lhs)))
    def cos[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("cos", Unwrap(lhs)))
    def tanh[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("tanh", Unwrap(lhs)))
    def exp[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("exp", Unwrap(lhs)))
    def log[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("log", Unwrap(lhs)))
    def sqrt[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("sqrt", Unwrap(lhs)))
  }

  // private Logical object for code reuse
  private[this] object Logical {
    def unary_~[T:Manifest](lhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("~", Unwrap(lhs)))
    def &[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("&", Unwrap(lhs), Unwrap(rhs)))
    def |[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("|", Unwrap(lhs), Unwrap(rhs)))
    def ^[T:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("^", Unwrap(lhs), Unwrap(rhs)))
    def <<[T:Manifest](lhs: Rep[T], rhs: Rep[Int])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect("<<", Unwrap(lhs), Unwrap(rhs)))
    def >>[T:Manifest](lhs: Rep[T], rhs: Rep[Int])(implicit pos: SourceContext): Rep[T] =
      Wrap[T](Adapter.g.reflect(">>", Unwrap(lhs), Unwrap(rhs)))
    def >>>[T:Manifest](lhs: Rep[T], rhs: Rep[Int])(implicit pos: SourceContext): Rep[T] = Unwrap(rhs) match {
      case Backend.Const(64) => unit(0.asInstanceOf[T])
      case _ => Wrap[T](Adapter.g.reflect(">>>", Unwrap(lhs), Unwrap(rhs)))
    }
  }

  // extra implicit type conversions helpers
  implicit def doubleToInt(x: Double) = x.toInt
  implicit def doubleToFloat(x: Double) = x.toFloat
  implicit def doubleToLong(x: Double) = x.toLong
  implicit def floatToInt(x: Float) = x.toInt
  implicit def longToInt(x: Long) = x.toInt
  implicit def intToChar(x: Int) = x.toChar

  def cast_helper[X,Y](x: Rep[X])(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext) : Rep[Y] = x match {
    case Wrap(Backend.Const(x: X)) => Wrap[Y](Backend.Const(c(x)))
    case _ => Wrap[Y](Adapter.g.reflect("cast", Unwrap(x), Backend.Const(manifest[Y])))
  }

  // Char
  implicit def repToPrimitiveMathOpsCharOpsCls(x: Rep[Char])(implicit __pos: SourceContext): PrimitiveMathOpsCharOpsCls = new PrimitiveMathOpsCharOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsCharOpsCls(x: Char)(implicit __pos: SourceContext): PrimitiveMathOpsCharOpsCls = new PrimitiveMathOpsCharOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsCharOpsCls(x: Var[Char])(implicit __pos: SourceContext): PrimitiveMathOpsCharOpsCls = new PrimitiveMathOpsCharOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsCharOpsCls(val self: Rep[Char])(implicit __pos: SourceContext) {
    def -(rhs: Rep[Char])(implicit __pos: SourceContext,__imp1: Overloaded73): Rep[Char] = Arithmetic.minus[Char](self, rhs)

    def &(__arg1: Rep[Char])(implicit __pos: SourceContext,__imp1: Overloaded1): Rep[Char] = Logical.&(self, __arg1) // FIXME: scala would be Int
    def |(__arg1: Rep[Char])(implicit __pos: SourceContext,__imp1: Overloaded1): Rep[Char] = Logical.|(self, __arg1) // FIXME: scala would be Int)

    def toInt(implicit __pos: SourceContext): Rep[Int] = cast_helper[Char,Int](self)
    def toLong(implicit __pos: SourceContext): Rep[Long] = cast_helper[Char,Long](self)
  }

  // FIMXE(feiw) this overlaps with the implicit class of specific types
  implicit class GenericMathOpsCls[T:Numeric:Manifest](val self: Rep[T])(implicit __pos: SourceContext) {
    def unary_- = Arithmetic.unary_-[T](self)
    def +(rhs: Rep[T])(implicit __pos: SourceContext, __imp1: Overloaded1) = { Arithmetic.plus[T](self, rhs) }
    def -(rhs: Rep[T])(implicit __pos: SourceContext, __imp1: Overloaded1) = { Arithmetic.minus[T](self, rhs) }
    def *(rhs: Rep[T])(implicit __pos: SourceContext, __imp1: Overloaded1) = { Arithmetic.times[T](self, rhs) }
    def /(rhs: Rep[T])(implicit __pos: SourceContext, __imp1: Overloaded1) = { Arithmetic.divide[T](self, rhs) }
  }

  // Int
  object Int {
    def parseInt(s: Rep[String])(implicit __pos: SourceContext): Rep[Int] = ???
    def MaxValue(implicit pos: SourceContext): Rep[Int] = unit(scala.Int.MaxValue)
    def MinValue(implicit pos: SourceContext): Rep[Int] = unit(scala.Int.MinValue)
  }

  implicit def repToPrimitiveMathOpsIntOpsCls(x: Rep[Int])(implicit __pos: SourceContext): PrimitiveMathOpsIntOpsCls = new PrimitiveMathOpsIntOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsIntOpsCls(x: Int)(implicit __pos: SourceContext): PrimitiveMathOpsIntOpsCls = new PrimitiveMathOpsIntOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsIntOpsCls(x: Var[Int])(implicit __pos: SourceContext): PrimitiveMathOpsIntOpsCls = new PrimitiveMathOpsIntOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsIntOpsCls(val self: Rep[Int])(implicit __pos: SourceContext) {
    def unary_- = Arithmetic.unary_-[Int](self)

    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { Arithmetic.plus[Double](self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { Arithmetic.minus[Double](self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded73) = { Arithmetic.times[Double](self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded74) = { Arithmetic.divide[Double](self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { Arithmetic.plus[Double](self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { Arithmetic.minus[Double](self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded74) = { Arithmetic.times[Double](self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { Arithmetic.divide[Double](self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { Arithmetic.plus[Double](self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { Arithmetic.minus[Double](self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded75) = { Arithmetic.times[Double](self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded76) = { Arithmetic.divide[Double](self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { Arithmetic.plus[Float](self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { Arithmetic.minus[Float](self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded76) = { Arithmetic.times[Float](self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded77) = { Arithmetic.divide[Float](self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { Arithmetic.plus[Float](self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { Arithmetic.minus[Float](self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded77) = { Arithmetic.times[Float](self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { Arithmetic.divide[Float](self.toFloat, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { Arithmetic.plus[Float](self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { Arithmetic.minus[Float](self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded78) = { Arithmetic.times[Float](self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded79) = { Arithmetic.divide[Float](self.toFloat, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { Arithmetic.plus[Int](self, unit(rhs)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { Arithmetic.minus[Int](self, unit(rhs)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded79) = { Arithmetic.times[Int](self, unit(rhs)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded80) = { Arithmetic.divide[Int](self, unit(rhs)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { Arithmetic.plus[Int](self, rhs) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { Arithmetic.minus[Int](self, rhs) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded80) = { Arithmetic.times[Int](self, rhs) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { Arithmetic.divide[Int](self, rhs) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { Arithmetic.plus[Int](self, readVar(rhs)) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { Arithmetic.minus[Int](self, readVar(rhs)) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded81) = { Arithmetic.times[Int](self, readVar(rhs)) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded82) = { Arithmetic.divide[Int](self, readVar(rhs)) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { Arithmetic.plus[Long](self.toLong, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { Arithmetic.minus[Long](self.toLong, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded82) = { Arithmetic.times[Long](self.toLong, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded83) = { Arithmetic.divide[Long](self.toLong, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { Arithmetic.plus[Long](self.toLong, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { Arithmetic.minus[Long](self.toLong, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded83) = { Arithmetic.times[Long](self.toLong, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { Arithmetic.divide[Long](self.toLong, rhs) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { Arithmetic.plus[Long](self.toLong, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { Arithmetic.minus[Long](self.toLong, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded84) = { Arithmetic.times[Long](self.toLong, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded85) = { Arithmetic.divide[Long](self.toLong, readVar(rhs)) }
    def %(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Arithmetic.mod[Int](self, __arg1)

    def &(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.&(self, __arg1)
    def |(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.|(self, __arg1)
    def ^(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.^(self, __arg1)
    def <<(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.<<(self, __arg1)
    def >>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.>>(self, __arg1)
    def >>>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.>>>(self, __arg1)
    def unary_~(implicit __pos: SourceContext) = Logical.unary_~(self)

    def toLong(implicit __pos: SourceContext) = cast_helper[Int,Long](self)
    def toDouble(implicit __pos: SourceContext) = cast_helper[Int,Double](self)
    def toFloat(implicit __pos: SourceContext) = cast_helper[Int,Float](self)
    def toChar(implicit __pos: SourceContext) = cast_helper[Int,Char](self)
  }

  // Long
  object Long {
    def parseLong(s: Rep[String])(implicit pos: SourceContext): Rep[Long] = ???
    def MaxValue(implicit pos: SourceContext): Rep[Long] = unit(scala.Long.MaxValue)
    def MinValue(implicit pos: SourceContext): Rep[Long] = unit(scala.Long.MinValue)
  }

  implicit def repToPrimitiveMathOpsLongOpsCls(x: Rep[Long])(implicit __pos: SourceContext): PrimitiveMathOpsLongOpsCls = new PrimitiveMathOpsLongOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsLongOpsCls(x: Long)(implicit __pos: SourceContext): PrimitiveMathOpsLongOpsCls = new PrimitiveMathOpsLongOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsLongOpsCls(x: Var[Long])(implicit __pos: SourceContext): PrimitiveMathOpsLongOpsCls = new PrimitiveMathOpsLongOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsLongOpsCls(val self: Rep[Long])(implicit __pos: SourceContext) {
    def unary_- = Arithmetic.unary_-[Long](self)

    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { Arithmetic.plus[Double](self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { Arithmetic.minus[Double](self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded105) = { Arithmetic.times[Double](self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded106) = { Arithmetic.divide[Double](self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { Arithmetic.plus[Double](self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { Arithmetic.minus[Double](self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded106) = { Arithmetic.times[Double](self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { Arithmetic.divide[Double](self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { Arithmetic.plus[Double](self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { Arithmetic.minus[Double](self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded107) = { Arithmetic.times[Double](self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded108) = { Arithmetic.divide[Double](self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { Arithmetic.plus[Float](self.toFloat, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { Arithmetic.minus[Float](self.toFloat, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded108) = { Arithmetic.times[Float](self.toFloat, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded109) = { Arithmetic.divide[Float](self.toFloat, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { Arithmetic.plus[Float](self.toFloat, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { Arithmetic.minus[Float](self.toFloat, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded109) = { Arithmetic.times[Float](self.toFloat, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { Arithmetic.divide[Float](self.toFloat, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { Arithmetic.plus[Float](self.toFloat, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { Arithmetic.minus[Float](self.toFloat, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded110) = { Arithmetic.times[Float](self.toFloat, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded111) = { Arithmetic.divide[Float](self.toFloat, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { Arithmetic.plus[Long](self, unit(rhs.toLong)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { Arithmetic.minus[Long](self, unit(rhs.toLong)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded111) = { Arithmetic.times[Long](self, unit(rhs.toLong)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded112) = { Arithmetic.divide[Long](self, unit(rhs.toLong)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { Arithmetic.plus[Long](self, rhs.toLong) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { Arithmetic.minus[Long](self, rhs.toLong) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded112) = { Arithmetic.times[Long](self, rhs.toLong) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { Arithmetic.divide[Long](self, rhs.toLong) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { Arithmetic.plus[Long](self, readVar(rhs).toLong) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { Arithmetic.minus[Long](self, readVar(rhs).toLong) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded113) = { Arithmetic.times[Long](self, readVar(rhs).toLong) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded114) = { Arithmetic.divide[Long](self, readVar(rhs).toLong) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { Arithmetic.plus[Long](self, unit(rhs)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { Arithmetic.minus[Long](self, unit(rhs)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded114) = { Arithmetic.times[Long](self, unit(rhs)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded115) = { Arithmetic.divide[Long](self, unit(rhs)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { Arithmetic.plus[Long](self, rhs) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { Arithmetic.minus[Long](self, rhs) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded115) = { Arithmetic.times[Long](self, rhs) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { Arithmetic.divide[Long](self, rhs) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { Arithmetic.plus[Long](self, readVar(rhs)) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { Arithmetic.minus[Long](self, readVar(rhs)) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded116) = { Arithmetic.times[Long](self, readVar(rhs)) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded117) = { Arithmetic.divide[Long](self, readVar(rhs)) }

    def %(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded1) = Arithmetic.mod(self, __arg1)
    def &(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.&[Long](self, __arg1)
    def |(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.|[Long](self, __arg1)
    def ^(__arg1: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.^[Long](self, __arg1)
    def <<(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.<<[Long](self, __arg1)
    def >>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.>>[Long](self, __arg1)
    def >>>(__arg1: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded1) = Logical.>>>[Long](self, __arg1)

    def toInt(implicit __pos: SourceContext) = cast_helper[Long,Int](self)
    def toDouble(implicit __pos: SourceContext) = cast_helper[Long,Double](self)
    def toFloat(implicit __pos: SourceContext) = cast_helper[Long,Float](self)
  }

  // Float
  object Float {
    def parseFloat(s: Rep[String])(implicit pos: SourceContext): Rep[Float] = ???
    def MaxValue(implicit pos: SourceContext): Rep[Float] = unit(scala.Float.MaxValue)
    def MinPositiveValue(implicit pos: SourceContext): Rep[Float] = unit(scala.Float.MinPositiveValue)
    def MinValue(implicit pos: SourceContext): Rep[Float] = unit(scala.Float.MinValue)
    def NaN(implicit pos: SourceContext): Rep[Float] = unit(scala.Float.MinValue)
    def NegativeInfinity(implicit pos: SourceContext): Rep[Float] = unit(scala.Float.NegativeInfinity)
    def PositiveInfinity(implicit pos: SourceContext): Rep[Float] = unit(scala.Float.PositiveInfinity)
  }

  implicit def repToPrimitiveMathOpsFloatOpsCls(x: Rep[Float])(implicit __pos: SourceContext): PrimitiveMathOpsFloatOpsCls = new PrimitiveMathOpsFloatOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsFloatOpsCls(x: Float)(implicit __pos: SourceContext): PrimitiveMathOpsFloatOpsCls = new PrimitiveMathOpsFloatOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsFloatOpsCls(x: Var[Float])(implicit __pos: SourceContext): PrimitiveMathOpsFloatOpsCls = new PrimitiveMathOpsFloatOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsFloatOpsCls(val self: Rep[Float])(implicit __pos: SourceContext) {
    def unary_- = Arithmetic.unary_-[Float](self)

    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { Arithmetic.plus[Double](self.toDouble, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { Arithmetic.minus[Double](self.toDouble, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded41) = { Arithmetic.times[Double](self.toDouble, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded42) = { Arithmetic.divide[Double](self.toDouble, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { Arithmetic.plus[Double](self.toDouble, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { Arithmetic.minus[Double](self.toDouble, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded42) = { Arithmetic.times[Double](self.toDouble, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { Arithmetic.divide[Double](self.toDouble, rhs) }
    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { Arithmetic.plus[Double](self.toDouble, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { Arithmetic.minus[Double](self.toDouble, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded43) = { Arithmetic.times[Double](self.toDouble, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded44) = { Arithmetic.divide[Double](self.toDouble, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { Arithmetic.plus[Float](self, unit(rhs)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { Arithmetic.minus[Float](self, unit(rhs)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded44) = { Arithmetic.times[Float](self, unit(rhs)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded45) = { Arithmetic.divide[Float](self, unit(rhs)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { Arithmetic.plus[Float](self, rhs) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { Arithmetic.minus[Float](self, rhs) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded45) = { Arithmetic.times[Float](self, rhs) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { Arithmetic.divide[Float](self, rhs) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { Arithmetic.plus[Float](self, readVar(rhs)) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { Arithmetic.minus[Float](self, readVar(rhs)) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded46) = { Arithmetic.times[Float](self, readVar(rhs)) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded47) = { Arithmetic.divide[Float](self, readVar(rhs)) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { Arithmetic.plus[Float](self, unit(rhs.toFloat)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { Arithmetic.minus[Float](self, unit(rhs.toFloat)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded47) = { Arithmetic.times[Float](self, unit(rhs.toFloat)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded48) = { Arithmetic.divide[Float](self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { Arithmetic.plus[Float](self, rhs.toFloat) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { Arithmetic.minus[Float](self, rhs.toFloat) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded48) = { Arithmetic.times[Float](self, rhs.toFloat) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { Arithmetic.divide[Float](self, rhs.toFloat) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { Arithmetic.plus[Float](self, readVar(rhs).toFloat) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { Arithmetic.minus[Float](self, readVar(rhs).toFloat) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded49) = { Arithmetic.times[Float](self, readVar(rhs).toFloat) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded50) = { Arithmetic.divide[Float](self, readVar(rhs).toFloat) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { Arithmetic.plus[Float](self, unit(rhs.toFloat)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { Arithmetic.minus[Float](self, unit(rhs.toFloat)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded50) = { Arithmetic.times[Float](self, unit(rhs.toFloat)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded51) = { Arithmetic.divide[Float](self, unit(rhs.toFloat)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { Arithmetic.plus[Float](self, rhs.toFloat) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { Arithmetic.minus[Float](self, rhs.toFloat) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded51) = { Arithmetic.times[Float](self, rhs.toFloat) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { Arithmetic.divide[Float](self, rhs.toFloat) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { Arithmetic.plus[Float](self, readVar(rhs).toFloat) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { Arithmetic.minus[Float](self, readVar(rhs).toFloat) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded52) = { Arithmetic.times[Float](self, readVar(rhs).toFloat) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded53) = { Arithmetic.divide[Float](self, readVar(rhs).toFloat) }

    def toInt(implicit __pos: SourceContext) = cast_helper[Float,Int](self)
    def toDouble(implicit __pos: SourceContext) = cast_helper[Float,Double](self)
  }

  // Double
  object Double {
    def parseDouble(s:Rep[String])(implicit __pos: SourceContext) = ???
    def PositiveInfinity(implicit pos: SourceContext) = unit(scala.Double.PositiveInfinity)
    def NegativeInfinity(implicit pos: SourceContext) = unit(scala.Double.NegativeInfinity)
    def NaN(implicit pos: SourceContext) = unit(scala.Double.NaN)
    def MinValue(implicit pos: SourceContext) = unit(scala.Double.MinValue)
    def MaxValue(implicit pos: SourceContext) = unit(scala.Double.MaxValue)
    def MinPositiveValue(implicit pos: SourceContext) = unit(scala.Double.MinPositiveValue)
  }

  implicit def repToPrimitiveMathOpsDoubleOpsCls(x: Rep[Double])(implicit __pos: SourceContext): PrimitiveMathOpsDoubleOpsCls = new PrimitiveMathOpsDoubleOpsCls(x)(__pos)
  implicit def liftToPrimitiveMathOpsDoubleOpsCls(x: Double)(implicit __pos: SourceContext): PrimitiveMathOpsDoubleOpsCls = new PrimitiveMathOpsDoubleOpsCls(unit(x))(__pos)
  implicit def varToPrimitiveMathOpsDoubleOpsCls(x: Var[Double])(implicit __pos: SourceContext): PrimitiveMathOpsDoubleOpsCls = new PrimitiveMathOpsDoubleOpsCls(readVar(x))(__pos)

  class PrimitiveMathOpsDoubleOpsCls(val self: Rep[Double])(implicit __pos: SourceContext) {
    def unary_- = Arithmetic.unary_-[Double](self)

    def +(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { Arithmetic.plus[Double](self, unit(rhs)) }
    def -(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { Arithmetic.minus[Double](self, unit(rhs)) }
    def *(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded9) = { Arithmetic.times[Double](self, unit(rhs)) }
    def /(rhs: Double)(implicit __pos: SourceContext,__imp1: Overloaded10) = { Arithmetic.divide[Double](self, unit(rhs)) }
    def +(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { Arithmetic.plus[Double](self, rhs) }
    def -(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { Arithmetic.minus[Double](self, rhs) }
    def *(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded10) = { Arithmetic.times[Double](self, rhs) }
    def /(rhs: Rep[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { Arithmetic.divide[Double](self, rhs) }

    def +(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { Arithmetic.plus[Double](self, readVar(rhs)) }
    def -(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { Arithmetic.minus[Double](self, readVar(rhs)) }
    def *(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded11) = { Arithmetic.times[Double](self, readVar(rhs)) }
    def /(rhs: Var[Double])(implicit __pos: SourceContext,__imp1: Overloaded12) = { Arithmetic.divide[Double](self, readVar(rhs)) }
    def +(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { Arithmetic.plus[Double](self, unit(rhs.toDouble)) }
    def -(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { Arithmetic.minus[Double](self, unit(rhs.toDouble)) }
    def *(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded12) = { Arithmetic.times[Double](self, unit(rhs.toDouble)) }
    def /(rhs: Float)(implicit __pos: SourceContext,__imp1: Overloaded13) = { Arithmetic.divide[Double](self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { Arithmetic.plus[Double](self, rhs.toDouble) }
    def -(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { Arithmetic.minus[Double](self, rhs.toDouble) }
    def *(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded13) = { Arithmetic.times[Double](self, rhs.toDouble) }
    def /(rhs: Rep[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { Arithmetic.divide[Double](self, rhs.toDouble) }
    def +(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { Arithmetic.plus[Double](self, readVar(rhs).toDouble) }
    def -(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { Arithmetic.minus[Double](self, readVar(rhs).toDouble) }
    def *(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded14) = { Arithmetic.times[Double](self, readVar(rhs).toDouble) }
    def /(rhs: Var[Float])(implicit __pos: SourceContext,__imp1: Overloaded15) = { Arithmetic.divide[Double](self, readVar(rhs).toDouble) }
    def +(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { Arithmetic.plus[Double](self, unit(rhs.toDouble)) }
    def -(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { Arithmetic.minus[Double](self, unit(rhs.toDouble)) }
    def *(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded15) = { Arithmetic.times[Double](self, unit(rhs.toDouble)) }
    def /(rhs: Int)(implicit __pos: SourceContext,__imp1: Overloaded16) = { Arithmetic.divide[Double](self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { Arithmetic.plus[Double](self, rhs.toDouble) }
    def -(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { Arithmetic.minus[Double](self, rhs.toDouble) }
    def *(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded16) = { Arithmetic.times[Double](self, rhs.toDouble) }
    def /(rhs: Rep[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { Arithmetic.divide[Double](self, rhs.toDouble) }
    def +(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { Arithmetic.plus[Double](self, readVar(rhs).toDouble) }
    def -(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { Arithmetic.minus[Double](self, readVar(rhs).toDouble) }
    def *(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded17) = { Arithmetic.times[Double](self, readVar(rhs).toDouble) }
    def /(rhs: Var[Int])(implicit __pos: SourceContext,__imp1: Overloaded18) = { Arithmetic.divide[Double](self, readVar(rhs).toDouble) }
    def +(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { Arithmetic.plus[Double](self, unit(rhs.toDouble)) }
    def -(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { Arithmetic.minus[Double](self, unit(rhs.toDouble)) }
    def *(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded18) = { Arithmetic.times[Double](self, unit(rhs.toDouble)) }
    def /(rhs: Long)(implicit __pos: SourceContext,__imp1: Overloaded19) = { Arithmetic.divide[Double](self, unit(rhs.toDouble)) }
    def +(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { Arithmetic.plus[Double](self, rhs.toDouble) }
    def -(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { Arithmetic.minus[Double](self, rhs.toDouble) }
    def *(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded19) = { Arithmetic.times[Double](self, rhs.toDouble) }
    def /(rhs: Rep[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { Arithmetic.divide[Double](self, rhs.toDouble) }
    def +(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { Arithmetic.plus[Double](self, readVar(rhs).toDouble) }
    def -(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { Arithmetic.minus[Double](self, readVar(rhs).toDouble) }
    def *(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded20) = { Arithmetic.times[Double](self, readVar(rhs).toDouble) }
    def /(rhs: Var[Long])(implicit __pos: SourceContext,__imp1: Overloaded21) = { Arithmetic.divide[Double](self, readVar(rhs).toDouble) }

    def toInt(implicit __pos: SourceContext) = cast_helper[Double,Int](self)
    def toFloat(implicit __pos: SourceContext) = cast_helper[Double,Float](self)
    def toLong(implicit __pos: SourceContext) = cast_helper[Double,Long](self)
  }

}


/**
 * Here we show how a Domain Specific Language (DSL) is built.
 * 1. Define a Dsl trait that collects the frontend trait needed (richer DSL can simply extend this Dsl with more traits)
 * 2. Define a DslDriver that extends the Dsl (DslExp in this case)
 *                       has a `val codegen` of DslGen where the IR in DslGen binds to the DslDrive itself via
 *                          ``` val IR: q.type = q ```
 * 3. For extension, define a DslDriverX that extends DslDriver with more frontent traits.
 *                   has a `val codegen` that is new DslGen with more codegen traits.
 */
trait Dsl extends PrimitiveOps with LiftPrimitives with Equal with RangeOps with OrderingOps with lms.collection.mutable.ArrayOps with UtilOps {

  class SeqOpsCls[T](x: Rep[Seq[Char]])
  implicit def repStrToSeqOps(a: Rep[String]) = new SeqOpsCls(a.asInstanceOf[Rep[Seq[Char]]])
}
trait DslExp extends Dsl // For backward compatibility
trait DslCPP extends Dsl with CPPOps


trait DslGen extends ExtendedScalaCodeGen {
  val IR: Base
  import IR._
  implicit class CodegenHelper(sc: StringContext) {
    def src(args: Any*): String = ???
  }
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = ???
  def emitSource[A: Manifest, B: Manifest](f: Rep[A]=>Rep[B], className: String, stream: java.io.PrintStream): List[(Class[_], Any)] = {
    val statics = Adapter.emitCommon1(className, this, stream)(manifest[A], manifest[B])(x => Unwrap(f(Wrap[A](x))))
    statics.toList
  }
}

trait DslGenC extends ExtendedCCodeGen {
  val IR: Base
  import IR._

  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = ???

  def emitSource[A : Manifest, B : Manifest](f: Rep[A]=>Rep[B], className: String, stream: java.io.PrintStream): List[(Class[_], Any)] = {
    val statics = Adapter.emitCommon1(className, this, stream)(manifest[A], manifest[B])(x => Unwrap(f(Wrap[A](x))))
    statics.toList
  }
}

trait DslGenCPP extends DslGenC with ExtendedCPPCodeGen

abstract class DslSnippet[A:Manifest, B:Manifest] extends Dsl {
  def wrapper(x: Rep[A]): Rep[B] = snippet(x)
  def snippet(x: Rep[A]): Rep[B]
}

// Basic DslDriver for Scala CodeGen
abstract class DslDriver[A:Manifest,B:Manifest] extends DslSnippet[A,B] with DslExp { q =>
  val codegen = new DslGen {
    val IR: q.type = q
  }
  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))
    (source.toString, statics)
  }
  lazy val f = { val (c1,s1) = (code,statics); time("scalac") { Global.sc.compile[A,B]("Snippet", c1, s1) }}

  def precompile: Unit = f

  def precompileSilently: Unit = utils.devnull(f)

  def eval(x: A): B = { val f1 = f; time("eval")(f1(x)) }
}

// Basic DslDriverC for C CodeGen
abstract class DslDriverC[A: Manifest, B: Manifest] extends DslSnippet[A, B] with DslExp { q =>
  val codegen = new DslGenC {
    val IR: q.type = q
  }
  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))
    (source.toString, statics)
  }
  val compilerCommand = "cc -std=c99 -O3"
  def libraries = codegen.libraryFlags mkString(" ")

  val sourceFile = "/tmp/snippet.c"
  val executable = "/tmp/snippet"
  lazy val f: A => Unit = {
    // TBD: should read result of type B?
    val out = new java.io.PrintStream(sourceFile)
    out.println(code)
    out.close
    (new java.io.File(executable)).delete
    import scala.sys.process._
    val includes = codegen.joinPaths(codegen.includePaths, "-I")
    val libraryPaths = codegen.joinPaths(codegen.libraryPaths, "-L")
    val pb: ProcessBuilder = s"$compilerCommand $sourceFile -o $executable $libraries $includes $libraryPaths"
    time("gcc") { pb.lines.foreach(Console.println _) }
    (a: A) => (s"$executable $a": ProcessBuilder).lines.foreach(Console.println _)
  }
  def eval(a: A): Unit = { val f1 = f; time("eval")(f1(a)) }
}

// Basic DslDriverCPP for CPP CodeGen
abstract class DslDriverCPP[A: Manifest, B: Manifest] extends DslDriverC[A, B] with CPPOps { q =>
  override val codegen = new DslGenCPP {
    val IR: q.type = q
  }
  override val compilerCommand = "g++ -std=c++17 -O3"
}

abstract class CompilerC[A:Manifest, B:Manifest] extends DslDriverC[A, B] { q =>

  // get original graph
  val initial_graph = Adapter.genGraph1(manifest[A], manifest[B])(x => Unwrap(wrapper(Wrap[A](x))))

  // run some transformation
  val passes: List[Transformer] = List()
  def transformOnePass(pass: Transformer, index: Int, graph: Graph): Graph = pass.transform(graph)
  def transform(graph: Graph): Graph =
    passes.zipWithIndex.foldLeft(graph) {case (graph, (pass, index)) => transformOnePass(pass, index, graph)}

  // codegen
  override lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = time("codegen") {
      val final_graph = transform(initial_graph)
      codegen.typeMap = Adapter.typeMap
      codegen.stream = new java.io.PrintStream(source)
      codegen.emitAll(final_graph, "Snippet")(manifest[A], manifest[B])
      codegen.extractAllStatics.toList
    }
    (source.toString, statics)
  }
}

// These empty traits have to be here for backward compatibility :(
trait MiscOps
trait StringOps
trait SeqOps
trait Functions
trait While
trait StaticData
trait Variables
trait ObjectOps
trait UncheckedOps
trait Timing
trait LiftVariables
trait NumericOps
trait BooleanOps
trait LiftString
trait LiftNumeric
trait LiftBoolean
trait IfThenElse

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
trait UncheckedOpsExp
trait UtilOpsExp

trait CGenBase extends DslGenC
trait ScalaGenBase extends DslGen
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

trait Compile
trait CompileScala
trait BaseExp
