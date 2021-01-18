package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorGemmTypeLess extends FixedSizeDistributedTensorBaseTypeLess {

  def Dot(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = (x.shape_size.size, y.shape_size.size) match {
      case (1,1) => // vector-vector-dot
        assert(x.shape_size == y.shape_size)
        TensorType(Seq(Size(Dim(next_dim_name), 1)), x.et)
      case (2,1) => // matrix-vector-dot
        assert(x.shape_size(1) == y.shape_size(0))
        TensorType(x.tensor_type.shape.take(1), x.et)
      case (2,2) => // matrix-matrix-dot
        assert(x.shape_size(1) == y.shape_size(0))
        TensorType(Seq(x.tensor_type.shape(0), y.tensor_type.shape(1)), x.et)
      case _ => throw new Exception("not yet supporting high dimension dot")
    }
    assert(x.et == y.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_dot", C(res_tt), C(anno), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }

  def DotWithTranspose(x: TENSOR, y: TENSOR, anno: Anno = NAnno, transL: Boolean = false, transR: Boolean = false)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = (x.shape_size.size, y.shape_size.size) match {
      case (1,1) => // vector-vector-dot
        assert(!transL && !transR, "cannot transpose the operand in vector-vector-dot")
        assert(x.shape_size == y.shape_size)
        TensorType(Seq(Size(Dim(next_dim_name), 1)), x.et)
      case (2,1) => // matrix-vector-dot
        assert(!transL && !transR, "cannot transpose the operand in matrix-vector-dot for now")
        assert(x.shape_size(1) == y.shape_size(0))
        TensorType(x.tensor_type.shape.take(1), x.et)
      case (2,2) => // matrix-matrix-dot
        val left_check = if (transL) x.shape_size(0) else x.shape_size(1)
        val left_dim = if (transL) x.tensor_type.shape(1) else x.tensor_type.shape(0)
        val right_check = if (transR) x.shape_size(1) else x.shape_size(0)
        val right_dim = if (transR) y.tensor_type.shape(0) else y.tensor_type.shape(1)
        assert(left_check == right_check)
        TensorType(Seq(left_dim, right_dim), x.et)
      case _ => throw new Exception("not yet supporting high dimension dot")
    }
    assert (x.et == y.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_dot_with_transpose", C(res_tt), C(anno), C(transL), C(transR), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }
}


trait FixedSizeDistributedTensorOpsGemm extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsGemm[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def gemm(y: Rep[Tensor[T]], anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Dot(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    def gemmT(y: Rep[Tensor[T]], anno: Anno, transL: Boolean = false, transR: Boolean = false)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = DotWithTranspose(self, tensor(y), anno, transL, transR)
      Wrap[Tensor[T]](t.x)
    }
  }
}

