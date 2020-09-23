package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

/**
 * In this frondend design, we are building Tensor IR with fixed shape and device.
 *
 * In the first step, we are simply supporting GPU and CPU.
 * Both Tensor IR and Tensor computation IR have device attributes.
 * We hope to resolve tensor communication automatically during transformation.
 *    such that: tensors are moved to GPU or CPU based on the request from tensor computation
 *               tensors can be printed only from CPU.
 */
object FixedSizeDistributedTensorTypeLess {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import CUDATypeLess._

  abstract class Device
  case class CPU(x: Int) extends Device
  case class GPU(x: Int) extends Device
  object UnKnownD extends Device

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  var dim_name: Int = 0;
  def next_dim_name: Int = {
    dim_name += 1
    dim_name - 1
  }

  case class Dim(x: Int) // named dimension
  def dim = Dim(next_dim_name)
  case class Size(d: Dim, s: Int) // dim and length
  case class TensorType(s: Seq[Size], d: Device, et: Manifest[_]) // tensor type

  abstract class Anno
  object NAnno extends Anno
  case class SAnno(d: Dim, devices: Seq[Device]) extends Anno // spatial splitting annotation
  case class KAnno(p: Int) extends Anno // stacked pipelines
  case class QAnno(p: Int) extends Anno // queued pipelines

  def build_inputs(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device]) = {
    val tt = TensorType(shape.map(s => Size(dim, s)), UnKnownD, et) // FIXME(feiw) remove UnKnowD
    val anno = SAnno(tt.s(index).d, devices)
    (tt, anno)
  }

  def INPUT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val (tt, anno) = build_inputs(shape, et, index, devices)
    INPUT(tt, anno)
  }

  // deprecate
  def INPUT(shape: Seq[Int], et: Manifest[_], anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    INPUT(TensorType(shape.map(s => Size(dim, s)), UnKnownD, et), anno)
  }

  def INPUT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_input", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def WEIGHT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val (tt, anno) = build_inputs(shape, et, index, devices)
    WEIGHT(tt, anno)
  }

  // deprecate
  def WEIGHT(shape: Seq[Int], et: Manifest[_], anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    WEIGHT(TensorType(shape.map(s => Size(dim, s)), UnKnownD, et), anno)
  }

  def WEIGHT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_weight", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  class TENSOR(override val x: Backend.Exp, val useOldMetadata: Boolean = false) extends TOP(x) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def et: Manifest[_] = {
      if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)
    }

    def tensor_type: TensorType = {
      val gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, Backend.Const(tt:TensorType)::_, _)) if s.startsWith("tensor") => tt
        case a => throw new Exception(s"cannot find node $a")
      }
    }

    def annotation: Anno = {
      val gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, tt::Backend.Const(a:Anno)::_, _)) if s.startsWith("tensor") => a
        case a => throw new Exception(s"cannot find node $a")
      }
    }

    def shape_size: Seq[Int] = tensor_type.s.map(_.s)
    def device: Device = tensor_type.d
    def device_shape: (Device, Seq[Int]) = (device, shape_size)

    // def to(target_device: Device)(implicit __pos: SourceContext): TENSOR = {
    //   if (device == target_device) this
    //   else (new TENSOR(Adapter.g.reflect("tensor_sendrecv", C(shape), C(target_device),
    //     C(device), x))).withSrcType(__pos, et)
    // }

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("show_tensor", x)()(Adapter.CTRL))
    }

    def elemWiseNoBroadCasting(y: TENSOR, anno: Anno, __pos: SourceContext)(op: String): TENSOR = {
      assert(shape_size == y.shape_size)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect(op, C(tensor_type), C(anno), x, y.x))).withSrcType(__pos, et)
    }

    def + (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_add")

    def - (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_minus")

    def * (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_mult")

    def / (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_div")

    def dot(y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
      val res_tt = (shape_size.size, y.shape_size.size) match {
        case (1,1) => // vector-vector-dot
          assert(shape_size == y.shape_size)
          TensorType(Seq(Size(Dim(next_dim_name), 1)), UnKnownD, et)
        case (2,1) => // matrix-vector-dot
          assert(shape_size(1) == y.shape_size(0))
          TensorType(tensor_type.s.take(1), device, et)
        case (2,2) => // matrix-matrix-dot
          assert(shape_size(1) == y.shape_size(0))
          TensorType(Seq(tensor_type.s(0), y.tensor_type.s(1)), device, et)
        case _ => throw new Exception("not yet supporting high dimension dot")
      }
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflect("tensor_dot", C(res_tt), C(anno), x, y.x))).withSrcType(__pos, et)
    }
  }
}


// trait FixedSizeTensorDeviceOps extends Dsl with ArrayOps with CudaOps {

//   import PrimitiveTypeLess._
//   import ArrayTypeLess._
//   import FixedSizeTensorDeviceTypeLess._

//   def NewArray[T:Manifest](x: Rep[Int], device: Device)(implicit __pos: SourceContext): Rep[Array[T]] = {
//     Wrap[Array[T]](ARRAYD(new INT(Unwrap(x)), manifest[T], device).x)
//   }

//   /// Typed Frontend
//   class Tensor[+T]
//   object Tensor {
//     def apply[T:Numeric:Manifest](shape: Seq[Int], array: Rep[Array[T]], device: Device = CPU(0))(implicit __pos: SourceContext): Rep[Tensor[T]] = {
//       Wrap[Tensor[T]](TENSOR(shape, new ARRAY(Unwrap(array)), device).x)
//     }
//   }

//   def atGPU[T:Numeric:Manifest](clo: Device => Rep[Tensor[T]]): Rep[Tensor[T]] = {
//       clo(GPU(0)) // clo is supposed to run with an implicit Device argument
//   }
//   def atGPU[T:Numeric:Manifest](clo: Device => Seq[Rep[Tensor[T]]]): Seq[Rep[Tensor[T]]] = {
//       clo(GPU(0)) // clo is supposed to run with an implicit Device argument
//   }

//   def tensor[T:Numeric:Manifest](x: Rep[Tensor[T]]): TENSOR = new TENSOR(Unwrap(x))

//   implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
//     val self = tensor(x)

//     def shape: Seq[Int] = self.shape
//     def device: Device = self.device
//     def to(target_device: Device)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
//       Wrap[Tensor[T]](self.to(target_device).x)
//     }
//     def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)

//     def + (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
//       val t = self + tensor(y)
//       Wrap[Tensor[T]](t.x)
//     }
//     def - (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
//       val t = self - tensor(y)
//       Wrap[Tensor[T]](t.x)
//     }
//     def * (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
//       val t = self * tensor(y)
//       Wrap[Tensor[T]](t.x)
//     }
//     def / (y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
//       val t = self / tensor(y)
//       Wrap[Tensor[T]](t.x)
//     }

//     def dot(y: Rep[Tensor[T]])(implicit device: Device, __pos: SourceContext): Rep[Tensor[T]] = {
//       val t = self dot tensor(y)
//       Wrap[Tensor[T]](t.x)
//     }
//   }
// }
