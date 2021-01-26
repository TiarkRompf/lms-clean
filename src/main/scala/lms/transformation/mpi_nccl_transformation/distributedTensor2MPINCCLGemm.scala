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
      CUBLAS_SGEMM(CUBLAS_HANDLE, CUBLAS_OP_T, CUBLAS_OP_T, m, n, k, ONE, new ARRAY(left_operand), m, new ARRAY(right_operand), k, ZERO, array, m)
      array
    }
  // helper function for computing dot product in GPUs
  // the dot is for 2 matrices of shape (m x k) and (k x n), allowing left/right operand to be transposed
  def gpu_dot_array_with_transpose(size: Int, man: Manifest[_], device: INT, left_operand: Backend.Exp, transL: Boolean, right_operand: Backend.Exp, transR: Boolean, m: Int, n: Int, k: Int)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing DOT on GPU for size $size and type $man at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand with transpose options") {
      assert(man == manifest[Float], s"Using cublas for dot operation, which only support float element type, but got: ${man}")
      val array = gpu_array(size, man, device)
      // the transpose options should also take account into the column-major assumption of the cublas function
      CUBLAS_SGEMM(CUBLAS_HANDLE, if (transL) CUBLAS_OP_N else CUBLAS_OP_T, if (transR) CUBLAS_OP_N else CUBLAS_OP_T,
        m, n, k, ONE, new ARRAY(left_operand), m, new ARRAY(right_operand), k, ZERO, array, m)
      array
    }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_dot", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // get the shape of `left` and `right` tensor
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)
      assert(left_shape.size == 2 && right_shape.size == 2 && left_shape(1) == right_shape(0),
          s"shapes of tensor_dot are invalid left operand shape: $left_shape x right operand shape: $right_shape")

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      // then we should run this dot op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_dot_array(count2, m, myNCCLRank, left_operand, right_operand, left_shape(0), right_shape(1), left_shape(1)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(tt.shapeSize)
          val dot_res = gpu_dot_array(count, m, myNCCLRank, left_operand, right_operand, left_shape(0), right_shape(1), left_shape(1))
          NCCL_ALLREDUCE(m, dot_res, dot_res, SIZE_T(count), NCCL_SUM, myNCCLComm, myNCCLStream)
          dot_res.x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_dot")
      }

    case Node(s, "tensor_dot_with_transpose", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(transL:Boolean)::Backend.Const(transR:Boolean)::(left:Backend.Sym)::(right:Backend.Sym)::_,_) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // get the shape of `left` and `right` tensor
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)
      assert(left_shape.size == 2 && right_shape.size == 2, s"For now, only handles matrix matrix dot")

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      // then we should run this dot op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_dot_array_with_transpose(count2, m, myNCCLRank, left_operand, transL, right_operand, transR,
            if (transL) left_shape(1) else left_shape(0), if (transR) right_shape(0) else right_shape(1), if (transL) left_shape(0) else left_shape(1)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(tt.shapeSize)
          val dot_res = gpu_dot_array_with_transpose(count, m, myNCCLRank, left_operand, transL, right_operand, transR,
            if (transL) left_shape(1) else left_shape(0), if (transR) right_shape(0) else right_shape(1), if (transL) left_shape(0) else left_shape(1))
          NCCL_ALLREDUCE(m, dot_res, dot_res, SIZE_T(count), NCCL_SUM, myNCCLComm, myNCCLStream)
          dot_res.x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_dot_with_transpose")
      }

    case _ => super.transform(n)
  }
}
