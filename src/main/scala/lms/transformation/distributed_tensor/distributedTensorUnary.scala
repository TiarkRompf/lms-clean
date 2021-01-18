package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorUnaryTypeLess extends FixedSizeDistributedTensorBaseTypeLess {

  def Transpose(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    assert(tensor.shape_size.size == 2, "input of transpose must be 2D")
    val res_tt = TensorType(Seq(tensor.tensor_type.shape(1), tensor.tensor_type.shape(0)), tensor.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_transpose", C(res_tt), C(anno), tensor.x)(tensor.x))).withSrcType(__pos, tensor.et)
  }
}


trait FixedSizeDistributedTensorOpsUnary extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsUnary[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def trans(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Transpose(self, anno)
      Wrap[Tensor[T]](t.x)
    }
  }
}
