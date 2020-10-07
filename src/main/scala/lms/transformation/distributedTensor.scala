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

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import CUDATypeLess._

  abstract class Device
  object UnKnownD extends Device
  case class CPU(x: Int) extends Device
  case class GPU(x: Int) extends Device

  var dim_name: Int = 0;
  def next_dim_name: Int = {
    dim_name += 1
    dim_name - 1
  }

  case class Dim(x: Int) // named dimension
  def dim = Dim(next_dim_name) // fresh dim
  case class Size(dim: Dim, size: Int) // dim and length
  case class TensorType(shape: Seq[Size], et: Manifest[_], anno: Anno = NAnno) { // tensor type
    def namedDim(x: Int) = shape(x).dim
  }

  abstract class Anno extends Serializable // Annotation class
  object NAnno extends Anno { // Null Annotation
    override def toString = "NAnno"
  }
  case class SAnno(dim: Dim, devices: Seq[Device], stable: Boolean = true) extends Anno // spatial splitting annotation
  case class KAnno(pipeline: Int) extends Anno // stacked pipelines
  case class QAnno(pipeline: Int) extends Anno // queued pipelines

  def INPUT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val tensorType = TensorType(shape.map(s => Size(dim, s)), et) // this is using default NAnno
    val anno = SAnno(tensorType.namedDim(index), devices)
    INPUT(tensorType, anno)
  }

  def INPUT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_input", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def WEIGHT(shape: Seq[Int], et: Manifest[_], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): TENSOR = {
    val tensorType = TensorType(shape.map(s => Size(dim, s)), et) // this is using default NAnno
    val anno = SAnno(tensorType.namedDim(index), devices)
    WEIGHT(tensorType, anno)
  }

  def WEIGHT(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_weight", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def ONES(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_ones", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  def ZEROS(tt: TensorType, anno: Anno)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectUnsafe("tensor_zeros", C(tt), C(anno)))).withSrcType(__pos, tt.et)
  }

  class TENSOR(override val x: Backend.Exp, val useOldMetadata: Boolean = false) extends TOP(x) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)

    def et: Manifest[_] = if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)

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

    def shape_size: Seq[Int] = tensor_type.shape.map(_.size)

    // def to(target_device: Device)(implicit __pos: SourceContext): TENSOR = {
    //   if (device == target_device) this
    //   else (new TENSOR(Adapter.g.reflect("tensor_sendrecv", C(shape), C(target_device),
    //     C(device), x))).withSrcType(__pos, et)
    // }

    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("show_tensor", x)(x)(Adapter.CTRL))
    }

    def += (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("accum_tensor", C(anno), x, y.x)(x, y.x)(x))
    }

    def optimize(grad: TENSOR, momentum: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectEffect("optimize_tensor", C(anno), x, grad.x, momentum.x)(x, grad.x, momentum.x)(x))
    }

    def elemWiseNoBroadCasting(y: TENSOR, anno: Anno, __pos: SourceContext)(op: String): TENSOR = {
      assert(shape_size == y.shape_size)
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflectRead(op, C(tensor_type), C(anno), x, y.x)(x, y.x))).withSrcType(__pos, et)
    }

    def + (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_add")

    def - (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_minus")

    def * (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_mult")

    def / (y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
      elemWiseNoBroadCasting(y, anno, __pos)("tensor_div")

    def transpose(anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
      assert(shape_size.size == 2, "input of transpose must be 2D")
      val res_tt = TensorType(Seq(tensor_type.shape(1), tensor_type.shape(0)), et)
      (new TENSOR(Adapter.g.reflectRead("tensor_transpose", C(res_tt), C(anno), x)(x))).withSrcType(__pos, et)
    }

    def dot(y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
      val res_tt = (shape_size.size, y.shape_size.size) match {
        case (1,1) => // vector-vector-dot
          assert(shape_size == y.shape_size)
          TensorType(Seq(Size(Dim(next_dim_name), 1)), et)
        case (2,1) => // matrix-vector-dot
          assert(shape_size(1) == y.shape_size(0))
          TensorType(tensor_type.shape.take(1), et)
        case (2,2) => // matrix-matrix-dot
          assert(shape_size(1) == y.shape_size(0))
          TensorType(Seq(tensor_type.shape(0), y.tensor_type.shape(1)), et)
        case _ => throw new Exception("not yet supporting high dimension dot")
      }
      assert(et == y.et)
      (new TENSOR(Adapter.g.reflectRead("tensor_dot", C(res_tt), C(anno), x, y.x)(x, y.x))).withSrcType(__pos, et)
    }
  }

  // FIXME(feiw) the return type fo MODULE is strange
  def MODULE(f: () => TENSOR)(implicit __pos: SourceContext): () => UNIT = {
    val m = Adapter.g.reflectWrite("module", Adapter.g.reify(f().x))(Adapter.CTRL)
    () => UNIT(Adapter.g.reflectWrite("@", m, Backend.Const(()))(Adapter.CTRL))
  }
}


trait FixedSizeDistributedTensorOps extends Dsl {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  def module[T](f: () => Rep[Tensor[T]])(implicit __pos: SourceContext) = {
    MODULE(() => new TENSOR(Unwrap(f())))
  }

  /// Typed Frontend
  class Tensor[+T]
  object Tensor {
    def input[T:Manifest](shape: Seq[Int], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(shape, manifest[T], index, devices)
      Wrap[Tensor[T]](tensor.x)
    }

    def input[T:Manifest](shape: Seq[Int])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(TensorType(shape.map(s => Size(dim, s)), manifest[T]), anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def input[T:Manifest](tensor_type: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = INPUT(tensor_type, anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int], index: Int, devices: Seq[Device])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(shape, manifest[T], index, devices)
      Wrap[Tensor[T]](tensor.x)
    }

    def weight[T:Manifest](shape: Seq[Int])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = WEIGHT(TensorType(shape.map(s => Size(dim, s)), manifest[T]), anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def ones[T:Manifest](tensor_type: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ONES(tensor_type, anno)
      Wrap[Tensor[T]](tensor.x)
    }

    def zeros[T:Manifest](tensor_type: TensorType)(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
      val tensor = ZEROS(tensor_type, anno)
      Wrap[Tensor[T]](tensor.x)
    }
  }

  def tensor_type[T:Numeric:Manifest](shape: Seq[Int]): TensorType =
    TensorType(shape.map(s => Size(dim, s)), manifest[T])

  def tensor[T:Numeric:Manifest](x: Rep[Tensor[T]]): TENSOR = new TENSOR(Unwrap(x))

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def shape: Seq[Int] = self.shape_size
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](self.show.x)

    // def + (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.+(y, anno)
    def + (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self + (tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def - (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.-(y, anno)
    def - (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self - (tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def * (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.*(y, anno)
    def * (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self * (tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def / (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this./(y, anno)
    def / (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self / (tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    def transpose(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self transpose anno
      Wrap[Tensor[T]](t.x)
    }

    // def dot(y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = {
    def dot(y: Rep[Tensor[T]], anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = self dot (tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }
  }
}
