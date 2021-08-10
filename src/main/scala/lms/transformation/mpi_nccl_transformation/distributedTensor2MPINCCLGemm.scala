package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._


trait DistributeTensor2MPI_NCCLGemm extends DistributeTensor2MPI_NCCLBase {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import RangeTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  import CUDATypeLess._
  import RandomDataTypeLess._
  import NCCLTypeLess._
  import SIZE_TTypeLess._
  import CUBLASTypeLess._

  // helper function for computing dot product in GPUs
  // the dot is for 2 matrices of shape (m x k) and (k x n)
  def gpu_dot_array(size: Int, man: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp, m: Int, n: Int, k: Int)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing DOT on GPU for size $size and type $man at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      assert(man == manifest[Float], s"Using cublas for dot operation, which only support float element type, but got: ${man}")
      val array = gpu_array(size, man, device)
      // we use transpose option because the cublas function assume column-major
      // CUBLAS_SGEMM(CUBLAS_HANDLE, CUBLAS_OP_T, CUBLAS_OP_T, m, n, k, VAR(ONE), new ARRAY(left_operand), k, new ARRAY(right_operand), n, VAR(ZERO), array, m)
      CUBLAS_SGEMM(CUBLAS_HANDLE, CUBLAS_OP_N, CUBLAS_OP_N, n, m, k, VAR(ONE), new ARRAY(right_operand), n, new ARRAY(left_operand), k, VAR(ZERO), array, n)
      array
    }
  // helper function for computing dot product in GPUs
  // the dot is for 2 matrices of shape (m x k) and (k x n), allowing left/right operand to be transposed
  def gpu_dot_array_with_transpose(size: Int, man: Manifest[_], device: INT, left_operand: Backend.Exp, transL: Boolean, right_operand: Backend.Exp, transR: Boolean, m: Int, n: Int, k: Int)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing DOT on GPU for size $size and type $man at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand with transpose options") {
      assert(man == manifest[Float], s"Using cublas for dot operation, which only support float element type, but got: ${man}")
      val array = gpu_array(size, man, device)
      // the transpose options should also take account into the column-major assumption of the cublas function
      // CUBLAS_SGEMM(CUBLAS_HANDLE, if (transL) CUBLAS_OP_N else CUBLAS_OP_T, if (transR) CUBLAS_OP_N else CUBLAS_OP_T,
      //   m, n, k, VAR(ONE), new ARRAY(left_operand), if (transL) m else k, new ARRAY(right_operand), if (transR) k else n, VAR(ZERO), array, m)
      CUBLAS_SGEMM(CUBLAS_HANDLE, if (transR) CUBLAS_OP_T else CUBLAS_OP_N, if (transL) CUBLAS_OP_T else CUBLAS_OP_N,
        n, m, k, VAR(ONE), new ARRAY(right_operand), if (transR) k else n, new ARRAY(left_operand), if (transL) m else k, VAR(ZERO), array, n)
      array
    }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_dot", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // get the shape of `left` and `right` tensor
      val leftTensorType = (new TENSOR(left, useOldMetadata = true)).resultType
      val rightTensorType = (new TENSOR(right, useOldMetadata = true)).resultType
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)

      assert(left_shape.size == 2 && right_shape.size == 2 && left_shape(1) == right_shape(0),
          s"shapes of tensor_dot are invalid left operand shape: $left_shape x right operand shape: $right_shape")

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val count2 = numeral(tt.shapeSize)
      val leftShape = leftTensorType.shapeSize
      val rightShape = rightTensorType.shapeSize
      gpu_dot_array(count2, m, myNCCLRank, transform(left), transform(right), leftShape(0), rightShape(1), leftShape(1)).x

    case Node(s, "tensor_dot_with_transpose", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(transL:Boolean)::Backend.Const(transR:Boolean)::(left:Backend.Sym)::(right:Backend.Sym)::_,_) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // get the shape of `left` and `right` tensor
      val leftTensorType = (new TENSOR(left, useOldMetadata = true)).resultType
      val rightTensorType = (new TENSOR(right, useOldMetadata = true)).resultType
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)
      assert(left_shape.size == 2 && right_shape.size == 2, s"For now, only handles matrix matrix dot")

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val count2 = numeral(tt.shapeSize)
      val leftShape = leftTensorType.shapeSize
      val rightShape = rightTensorType.shapeSize
      gpu_dot_array_with_transpose(count2, m, myNCCLRank, transform(left), transL, transform(right), transR,
        if (transL) leftShape(1) else leftShape(0), if (transR) rightShape(0) else rightShape(1), if (transL) leftShape(0) else leftShape(1)).x

    case _ => super.transform(n)
  }
}
