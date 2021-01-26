package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

object SIZE_TTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._

  def SIZE_OF(m: Manifest[_])(implicit __pos: SourceContext): INT = INT(Adapter.g.reflectUnsafe("sizeof", Backend.Const(m)))

  case class SizeT(x: Int) { override def toString() = x.toString }

  class SIZE_T(override val x: Backend.Exp) extends TOP(x) {
    this.withType(manifest[SizeT])
  }
  def SIZE_T(x: Backend.Exp)(implicit __pos: SourceContext): SIZE_T = (new SIZE_T(x)).withSource(__pos)
  implicit def SIZE_T(x: NUM)(implicit __pos: SourceContext): SIZE_T = {
    assert (x.t == manifest[Int])
    SIZE_T(Adapter.g.reflectUnsafe("cast", x.x, Backend.Const("SizeT")))
  }
}

trait SizeTOps extends Base with PrimitiveOps with CLibs {

  // NOTE(feiw) as a short-cut for constant int :)
  case class SizeT(x: Int) { override def toString() = x.toString }
  implicit def sizeTFromInt(x: Int) = SizeT(x)

  // NOTE(feiw) as a normal Rep[SizeT] construction from Rep[Int]
  def SizeT(x: Rep[Int]): Rep[SizeT] =
    Wrap[SizeT](Adapter.g.reflectUnsafe("cast", Unwrap(x), Backend.Const("SizeT")))

  // we offer SizeT to Int type casting as a method
  implicit def sizeTRepToOps(x: Rep[SizeT])(implicit __pos: SourceContext): SizeTOps = new SizeTOps(x)(__pos)
  implicit def sizeTVarToOps(x: Var[SizeT])(implicit __pos: SourceContext): SizeTOps = new SizeTOps(readVar(x))(__pos)
  class SizeTOps(x: Rep[SizeT])(implicit __pos: SourceContext) {
    def toInt: Rep[Int] = Wrap[Int](Adapter.g.reflect("cast", Unwrap(x), Backend.Const("Int")))
    def * (y: Rep[Int]) = Wrap[SizeT](Adapter.g.reflect("*", Unwrap(x), Unwrap(y)))
  }

  // void * memset ( void * ptr, int value, size_t num );
  // Note that the `value: Rep[Int]` is viewed as unsigned char
  def memset[T:Manifest](ptr: Rep[Array[T]], value: Rep[Int], num: Rep[SizeT]) =
    libFunction[Unit]("memset", Unwrap(ptr), Unwrap(value), Unwrap(num))(Seq(2), Seq(0), Set[Int]())
  def memsetOfT[T:Manifest](ptr: Rep[Array[T]], value: Rep[Int], count: Rep[Int])(implicit __pos: SourceContext) =
    memset(ptr, value, SizeT(count * sizeOf[T]))

  // void * memcpy ( void * destination, const void * source, size_t num );
  def memcpy[T:Manifest](destination: Rep[Array[T]], source: Rep[Array[T]], num: Rep[SizeT]) =
    libFunction[Unit]("memcpy", Unwrap(destination), Unwrap(source), Unwrap(num))(Seq(1, 2), Seq(0), Set[Int]())
  def memcpyOfT[T:Manifest](dst: Rep[Array[T]], src: Rep[Array[T]], count: Rep[Int])(implicit __pos: SourceContext) =
    memcpy[T](dst, src, SizeT(count * sizeOf[T]))

  // void* malloc( size_t size );
  def malloc[T:Manifest](size: Rep[SizeT]) =
    libFunction[Array[T]]("malloc", Unwrap(size))(Seq(0), Seq[Int](), Set[Int]())
  def mallocOfT[T:Manifest](count: Rep[Int])(implicit __pos: SourceContext) = malloc(SizeT(count * sizeOf[T]))

  // also add sizeOf here
  def sizeOf[T:Manifest]: Rep[Int] = Wrap[Int](Adapter.g.reflectUnsafe("sizeof", Backend.Const(manifest[T])))
}

trait CCodeGenSizeTOps extends ExtendedCCodeGen with CCodeGenLibs {

  registerHeader("<string.h>")

  override def remap(m: Manifest[_]): String = m.runtimeClass.getName match {
    case s: String if s.endsWith("SizeT") => "size_t"
    case _ => super.remap(m)
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "sizeof", List(Backend.Const(m:Manifest[_])), _) =>
      emit(s"sizeof(${remap(m)})")
    case _ => super.shallow(n)
  }
}
