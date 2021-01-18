package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorBinaryTypeLess extends FixedSizeDistributedTensorBaseTypeLess {

  def ElemWiseNoBroadCasting(x: TENSOR, y: TENSOR, anno: Anno, __pos: SourceContext)(op: String): TENSOR = {
    assert(x.shape_size == y.shape_size)
    assert(x.et == y.et)
    (new TENSOR(Adapter.g.reflectRead(op, C(x.tensor_type), C(anno), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }

  def Add(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_add")

  def Sub(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_minus")

  def Mul(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_mult")

  def Div(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_div")
}


trait FixedSizeDistributedTensorOpsBinary extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsBinary[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    // def + (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.+(y, anno)
    def + (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Add(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def - (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.-(y, anno)
    def - (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Sub(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def * (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.*(y, anno)
    def * (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Mul(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def / (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this./(y, anno)
    def / (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Div(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }
  }
}

