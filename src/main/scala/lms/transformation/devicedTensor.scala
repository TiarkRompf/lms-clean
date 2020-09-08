package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.ArrayCPUOps
import lms.thirdparty.{CudaOps, CUDATypeLess}

import Backend._

trait Devices {
  class Device
  case class CPU(x: Int) extends Device
  case class GPU(x: Int) extends Device
}

/**
 * In this frondend design, we are building Tensor IR with fixed shape and device.
 *
 * In the first step, we are simply supporting GPU and CPU.
 * Both Tensor IR and Tensor computation IR have device attributes.
 * We hope to resolve tensor communication automatically during transformation.
 *    such that: tensors are moved to GPU or CPU based on the request from tensor computation
 *               tensors can be printed only from CPU.
 */
object FixedSizeTensorDeviceTypeLess extends Devices {
  import BaseTypeLess._
  import ArrayTypeLess._
  import CUDATypeLess._

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  /// ARRAY Frontend // FIXME(should it be here?)
  def ARRAYD(size: Int, m: Manifest[_], d: Device)(implicit __pos: SourceContext): ARRAY = d match {
    case CPU(_) => (new ARRAY(Adapter.g.reflectMutable("NewArray", Backend.Const(size)))).withSrcType(__pos, m.arrayManifest)
    case GPU(_) => CUDA_MALLOC(size, m)
  }


  /// typeless frontend
  def TENSOR(shape: Seq[Int], array: ARRAY)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflect("tensor", C(shape), C(CPU(0)), array.x))).withSrcType(__pos, array.et)
  }

  class TENSOR(override val x: Backend.Exp) extends TOP(x) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def et: Manifest[_] = Adapter.typeMap(x)

    // Convention: every `tensor_*` IR has `shape` as the first Def and the `device` as the second Def
    def shape: Seq[Int] = shape(Adapter.g.globalDefsCache)
    def shape(graphCache: Map[Backend.Sym, Backend.Node]): Seq[Int] = {
      graphCache.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(size:Seq[Int])::_, _)) if s.startsWith("tensor") => size
        case a => System.out.println(a); ???
      }
    }

    def device: Device = device(Adapter.g.globalDefsCache)
    def device(graphCache: Map[Backend.Sym, Backend.Node]): Device = {
      graphCache.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, a::Backend.Const(d:Device)::_, _)) if s.startsWith("tensor") => d
        case a => System.out.println(a); ???
      }
    }

    def to(target_device: Device)(implicit __pos: SourceContext): TENSOR = {
      if (device == target_device) this
      else (new TENSOR(Adapter.g.reflect("tensor_sendrecv", C(shape), C(target_device), C(device), x))).withSrcType(__pos, et)
    }


    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectWrite("show_tensor", x)(Adapter.CTRL))
    }

    def + (y: TENSOR)(implicit device: Device, __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_add", C(shape), C(device), x, y.x))).withSrcType(__pos, et)
    }

    def - (y: TENSOR)(implicit device: Device, __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_minus", C(shape), C(device), x, y.x))).withSrcType(__pos, et)
    }

    def * (y: TENSOR)(implicit device: Device, __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_mult", C(shape), C(device), x, y.x))).withSrcType(__pos, et)
    }

    def / (y: TENSOR)(implicit device: Device, __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_div", C(shape), C(device), x, y.x))).withSrcType(__pos, et)
    }

    def dot(y: TENSOR)(implicit device: Device, __pos: SourceContext): TENSOR = {
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
      (new TENSOR(Adapter.g.reflect("tensor_dot", C(res_shape), C(device), x, y.x))).withSrcType(__pos, et)
    }
  }
}


trait FixedSizeTensorDeviceOps extends Dsl with ArrayOps with CudaOps {

  import ArrayTypeLess._
  import FixedSizeTensorDeviceTypeLess._

  /// Typed Frontend
  class Tensor[+T]
  object Tensor {
    def apply[T:Numeric:Manifest](shape: Seq[Int], array: Rep[Array[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      Wrap[Tensor[T]](TENSOR(shape, new ARRAY(Unwrap(array))).x)
    }
  }

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = new TENSOR(Unwrap(x))

    def shape: Seq[Int] = self.shape
    def device: Device = self.device
    def to(target_device: Device)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      Wrap[Tensor[T]](self.to(target_device).x)
    }
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)

    def + (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self + (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
    def - (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self - (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
    def * (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self * (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
    def / (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self / (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }

    def dot(y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self dot (new TENSOR(Unwrap(y)))
      Wrap[Tensor[T]](t.x)
    }
  }
}
