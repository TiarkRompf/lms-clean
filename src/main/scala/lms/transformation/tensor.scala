package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.ArrayCPUOps

import Backend._

/**
 * In this frontend design, we are showing a paradigm of building both the `typeless`
 * frontend (with all captilized letters: TENSOR), and the `typed` frontend (Tensor).
 *
 * Both typeless frontend and typed frontend will wrap `Backend.Exp` with Scala classes
 * and register the MetaData (including SourceContext and type Manifest).
 *
 * In this given design, the typed frontend actually uses the typeless frontend internally.
 * That is to say, the typed frontend is a shallow wrapper of the typeless frontend.
 * This is not super necessary but it should avoid duplication of logic in IR construction.
 *
 * We are trying to bring up the typeless frontend in other frontend traits as well, because
 * the typeless frontend is more friendly to IR transformations.
 * This is similar to how MLIR is typeless and has been easy to run transformations.
 */
trait FixedSizeTensorFrontEnd extends Base with PrimitiveOps with ArrayOps {

  /// typeless frontend
  // Note how the SourceContext and type Manifest are registered.
  // The `case class TENSOR` is used for `unsafe wrapping` (wrapping Backend.Exp as TENSOR)
  //      and the methods associated with TENSOR
  // The `def TENSOR` is used for `safe construction` of TENSOR, which registers metadata.
  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  class TENSOR(override val x: Backend.Exp) extends TOP(x) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def et: Manifest[_] = Adapter.typeMap(x)

    def shape: Seq[Int] = shape(Adapter.g.globalDefsCache)
    def shape(graphCache: Map[Backend.Sym, Backend.Node]): Seq[Int] = {
      graphCache.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(size:Seq[Int])::_, _)) if s.startsWith("tensor") => size
        case a => System.out.println(a); ???
      }
    }

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectWrite("show_tensor", x)(Adapter.CTRL))
    }

    def + (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_add", C(shape), x, y.x))).withSrcType(__pos, et)
    }

    def - (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_minus", C(shape), x, y.x))).withSrcType(__pos, et)
    }

    def * (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_mult", C(shape), x, y.x))).withSrcType(__pos, et)
    }

    def / (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_div", C(shape), x, y.x))).withSrcType(__pos, et)
    }

    def dot(y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      val res_shape = if (shape.size == 1 && y.shape.size == 1) {
        assert(shape == y.shape) // vector-vector-dot
        Seq(1)
      } else if (shape.size == 2 && y.shape.size == 1) {
        assert(shape(1) == y.shape(0)) // matrix-vector-dot
        Seq(shape(0))
      } else if (shape.size == 2 && y.shape.size == 2) {
        assert(shape(1) == y.shape(0)) // matrix-matrix-dot
        Seq(shape(0), y.shape(1))
      } else {
        assert(false)
      }
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_dot", C(res_shape), x, y.x))).withSrcType(__pos, et)
    }
  }

  def TENSOR(shape: Seq[Int], array: ARRAY)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflect("tensor", C(shape), array.x))).withSrcType(__pos, array.et)
  }


  /// typed frontend
  // Note how the typed frontend shallowly wrap the typeless frontend.
  // The `object Tensor` is used to construct a Rep[Tensor[T]]
  // The `implicit class TensorOps` is used to provide methods of Rep[Tensor[T]]
  class Tensor[+T]
  object Tensor {
    def apply[T:Numeric:Manifest](shape: Seq[Int], array: Rep[Array[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      // Whenever we have something that is Rep[T], it is OK to use `unsafe construction` (CLASS(Unwrap(a)))
      // to obtain the typeless object because the metadata must have been registered.

      // Then we use the safe construction of TENSOR to build TENSOR object, which is then
      // shallowly wrapped to Rep[Tensor[T]]
      Wrap[Tensor[T]](TENSOR(shape, new ARRAY(Unwrap(array))).x)
    }
  }

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {

    // first keep a local instance of TENSOR. No need to worry about registering metadata
    // here because we know that the metadata has ben registered when x: Rep[Tensor[T]] is constructed
    val self = new TENSOR(Unwrap(x))

    def shape: Seq[Int] = self.shape
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)
    def + (y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self + (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
    def - (y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self - (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
    def * (y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self * (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
    def / (y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self / (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }

    def dot(y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self dot (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
  }
}
