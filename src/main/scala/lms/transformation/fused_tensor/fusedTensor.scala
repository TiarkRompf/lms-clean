package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

object FusedTensorTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import CUDATypeLess._

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  /// typeless frontend
  def TENSOR(size: Int, array: ARRAY)(f: Backend.Exp => Backend.Exp)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectRead("tensor", C(size), array.x, Adapter.g.reify(xn => f(xn)))(array.x))).withSrcType(__pos, array.et)
  }

  def ZEROS(size: Int, array: ARRAY)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_zeros", C(size), array.x))).withSrcType(__pos, array.et)
  }

  def ONES(size: Int, array: ARRAY)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_ones", C(size), array.x))).withSrcType(__pos, array.et)
  }

  class TENSOR(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def et: Manifest[_] = {
      if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)
    }

    def size: Int = {
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(size:Int)::_, _)) => size
        case a => System.out.println(a); ???
      }
    }

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("show_tensor", x)()(Adapter.CTRL))
    }

    def + (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      (new TENSOR(Adapter.g.reflectUnsafe("tensor_add", x, y.x))).withSrcType(__pos, et)
    }

    def - (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      (new TENSOR(Adapter.g.reflect("tensor_minus", x, y.x))).withSrcType(__pos, et)
    }

    def apply(e: Backend.Exp)(implicit __pos: SourceContext): INT = {
      // todo: change to correct effect
      INT(Adapter.g.reflectEffect("tensor_apply", x, e)()(Adapter.CTRL)).withSrcType(__pos, et)
    }
  }
}


trait FusedTensorOps extends Dsl with ArrayOps with CudaOps {

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import FusedTensorTypeLess._

  // def NewArray[T:Manifest](x: Rep[Int])(implicit __pos: SourceContext): Rep[Array[T]] = {
  //   Wrap[Array[T]](ARRAY(new INT(Unwrap(x)), manifest[T]).x)
  // }

  /// Typed Frontend
  class Tensor[+T]
  object Tensor {
    def zeros[T:Manifest](size: Int, array: Rep[Array[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ZEROS(size, new ARRAY(Unwrap(array)))
      Wrap[Tensor[T]](tensor.x)
    }

    def ones[T:Manifest](size: Int, array: Rep[Array[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ONES(size, new ARRAY(Unwrap(array)))
      Wrap[Tensor[T]](tensor.x)
    }

    def apply[T:Numeric:Manifest](size: Int, array: Rep[Array[T]], f: Rep[Int] => Rep[Int])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      Wrap[Tensor[T]](TENSOR(size, new ARRAY(Unwrap(array)))(unwrapFun[Int, Int](f)).x)
    }
  }

  def tensor[T:Numeric:Manifest](x: Rep[Tensor[T]]): TENSOR = new TENSOR(Unwrap(x))

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)

    def + (y: Rep[Tensor[T]])(implicit  __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self + tensor(y)
      Wrap[Tensor[T]](t.x)
    }

    def - (y: Rep[Tensor[T]])(implicit  __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self - tensor(y)
      Wrap[Tensor[T]](t.x)
    }

    def apply(y: Rep[Int])(implicit  __pos: SourceContext): Rep[T] = {
      val t = self.apply(Unwrap(y))
      Wrap[T](t.x)
    }

  }
}
