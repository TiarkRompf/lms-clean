package lms.collection.mutable

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

trait ArrayOps { b: Base =>

  def NewArray[T:Manifest](x: Rep[Int]): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("NewArray", Unwrap(x)))
  }
  def Array[T:Manifest](xs: Rep[T]*): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("Array", xs.map(Unwrap(_)):_*))
  }
  implicit class ArrayOps[A:Manifest](x: Rep[Array[A]]) {
    def apply(i: Rep[Int]): Rep[A] = x match {
      case Wrap(_) => Wrap[A](Adapter.g.reflectRead("array_get", Unwrap(x), Unwrap(i))(Unwrap(x)))
      case EffectView(x, base) => Wrap[A](Adapter.g.reflectRead("array_get", Unwrap(x), Unwrap(i))(Unwrap(base)))
    }
    def update(i: Rep[Int], y: Rep[A]): Unit = x match {
      case Wrap(_) => Adapter.g.reflectWrite("array_set", Unwrap(x), Unwrap(i), Unwrap(y))(Unwrap(x))
      case EffectView(x, base) => Adapter.g.reflectWrite("array_set", Unwrap(x), Unwrap(i), Unwrap(y))(Unwrap(base))
    }
    def length: Rep[Int] = Wrap[Int](Adapter.g.reflect("array_length", Unwrap(x)))
    def slice(s: Rep[Int], e: Rep[Int]): Rep[Array[A]] = EffectView[Array[A]](Wrap[Array[A]](Adapter.g.reflect("array_slice", Unwrap(x), Unwrap(s), Unwrap(e))), x) // (Unwrap(x), Adapter.STORE)())
    def free: Unit = Adapter.g.reflectFree("array_free", Unwrap(x))(Unwrap(x))
    def copyToArray(arr: Rep[Array[A]], start: Rep[Int], len: Rep[Int]) = Adapter.g.reflectEffect("array_copyTo", Unwrap(x), Unwrap(arr), Unwrap(start), Unwrap(len))(Unwrap(x))(Unwrap(arr))
    def copyToLongArray(arr: Rep[LongArray[A]], start: Rep[Long], len: Rep[Int]) = Adapter.g.reflectEffect("array_copyTo", Unwrap(x), Unwrap(arr), Unwrap(start), Unwrap(len))(Unwrap(x))(Unwrap(arr))
    // FIXME: currently copy!!
    def sort(size: Rep[Int] /* , sort: (Rep[A], Rep[A]) => Rep[Int] */) = Wrap[Array[A]](Adapter.g.reflectMutable("array_sort_scala", Unwrap(x), Unwrap(size))) //, Adapter.g.reify((x, y) => Unwrap(sort(Wrap[A](x), Wrap[A](y)))))(Unwrap(x))(Unwrap(x))
  }

  trait LongArray[+T]
  def NewLongArray[T:Manifest](x: Rep[Long], init: Option[Int] = None): Rep[LongArray[T]] = init match {
    case Some(v) => Wrap[LongArray[T]](Adapter.g.reflectMutable("NewArray", Unwrap(x), Backend.Const(v)))
    case _ => Wrap[LongArray[T]](Adapter.g.reflectMutable("NewArray", Unwrap(x)))
  }
  def LongArray[T:Manifest](xs: Rep[T]*): Rep[LongArray[T]] = {
    Wrap[LongArray[T]](Adapter.g.reflectMutable("Array", xs.map(Unwrap(_)):_*))
  }
  implicit class LongArrayOps[A:Manifest](x: Rep[LongArray[A]]) {
    def apply(i: Rep[Long]): Rep[A] = x match {
      case Wrap(_) => Wrap[A](Adapter.g.reflectRead("array_get", Unwrap(x), Unwrap(i))(Unwrap(x)))
      case EffectView(x, base) => Wrap[A](Adapter.g.reflectRead("array_get", Unwrap(x), Unwrap(i))(Unwrap(base)))
    }
    def update(i: Rep[Long], y: Rep[A]): Unit = x match {
      case Wrap(_) => Adapter.g.reflectWrite("array_set", Unwrap(x), Unwrap(i), Unwrap(y))(Unwrap(x))
      case EffectView(x, base) => Adapter.g.reflectWrite("array_set", Unwrap(x), Unwrap(i), Unwrap(y))(Unwrap(base))
    }
    def length: Rep[Long] = Wrap[Long](Adapter.g.reflect("array_length", Unwrap(x)))
    def slice(s: Rep[Long], e: Rep[Long] = unit(-1L)): Rep[LongArray[A]] = EffectView[LongArray[A]](Wrap[LongArray[A]](Adapter.g.reflect("array_slice", Unwrap(x), Unwrap(s), Unwrap(e))), x) // FIXME: borrowing effect?
    def resize(s: Rep[Long]): Rep[LongArray[A]] = Wrap[LongArray[A]](Adapter.g.reflectRealloc("array_resize", Unwrap(x), Unwrap(s))(Unwrap(x)))
    def free: Unit = Adapter.g.reflectFree("array_free", Unwrap(x))(Unwrap(x))
  }
}

// FIXME(feiw) CodeGen of Array is still not yet separated from ExtendedScalaCodeGen and ExtendedCCodeGen
