package lms.collection.mutable

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext


object ArrayTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import RangeTypeLess._

  def ARRAY(size: Int, m: Manifest[_])(implicit __pos: SourceContext): ARRAY =
    (new ARRAY(Adapter.g.reflectMutable("NewArray", Backend.Const(size)))).withSrcType(__pos, m.arrayManifest)

  def ARRAY(x: TOP): ARRAY = new ARRAY(x.x)

  class ARRAY(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x, useOldMetadata) {
    def et: Manifest[_] = Adapter.typeMap(x).typeArguments.head
    def size: Int = {
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, "NewArray", Backend.Const(s:Int)::_, _)) => s
        case a => throw new Exception(s"Not an Array node: $a")
      }
    }

    def apply(i: INT)(implicit __pos: SourceContext): NUM =
      NUM(Adapter.g.reflectRead("array_get", x, i.x)(x), et)
    def update(i: INT, y: NUM)(implicit __pos: SourceContext): UNIT =
      UNIT(Adapter.g.reflectWrite("array_set", x, i.x, y.x)(x))

    def slice(size: INT, end: INT)(implicit __pos: SourceContext): ARRAY =
      (new ARRAY(Adapter.g.reflect("array_slice", x, size.x, end.x))).withSrcType(__pos, et.arrayManifest)

    def print(implicit __pos: SourceContext) = {

      val format =
        if (et == manifest[Int]) "%d "
        else if (et == manifest[Float]) "%f "
        else throw new Exception(s"type $et not yet handled in format")

      for (i <- RANGE_UNTIL(0, size)) {
        PRINTF(format, apply(i))
      }

      PRINTF("\n"); ()
    }
  }
}


trait ArrayOps extends PrimitiveOps {

  def NewArray[T:Manifest](x: Rep[Int]): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("NewArray", Unwrap(x)))
  }
  def Array[T:Manifest](xs: Rep[T]*): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("Array", xs.map(Unwrap(_)):_*))
  }

  implicit def repToArrayOps[A:Manifest](x: Rep[Array[A]]) = new ArrayOps(x)
  implicit def varToArrayOps[A:Manifest](x: Var[Array[A]]) = new ArrayOps(readVar(x))

  class ArrayOps[A:Manifest](x: Rep[Array[A]]) {
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
  implicit class CharArrayOps(x: Rep[Array[Char]]) {
    def ArrayOfCharToString(): Rep[String] = {
      Wrap[String](Adapter.g.reflectRead("array-of-char-to-string", Unwrap(x))(Unwrap(x)))
    }
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

// StackArray is the array using stack (i.e. int a[5] = {1,2,3,4,5};)
trait StackArrayOps extends ArrayOps { b: Base =>

  def NewStackArray[T:Manifest](x: Rep[Int]): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("NewStackArray", Unwrap(x)))
  }

  def StackArray[T:Manifest](xs: Rep[T]*): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("StackArray", xs.map(Unwrap(_)): _*))
  }
}

trait CCodeGenStackArray extends ExtendedCCodeGen {

  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "NewStackArray", List(x), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"$tpe "); shallow(s); emit("["); shallow(x); emitln("];")
    case n @ Node(s, "StackArray", xs, _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit(s"$tpe "); shallow(s); emit("[] = {"); shallow(xs.head)
      xs.tail.foreach(x => {emit(", "); shallow(x)}); emitln("};")
    case _ => super.traverse(n)
  }

  override def mayInline(n: Node): Boolean = n match {
    case Node(s, "NewStackArray", _, _) => false
    case Node(s, "StackArray", _, _) => false
    case _ => super.mayInline(n)
  }
}
