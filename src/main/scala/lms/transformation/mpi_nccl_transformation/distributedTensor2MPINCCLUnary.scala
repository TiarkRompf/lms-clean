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

trait DistributedTensor2MPI_NCCLUnary extends DistributeTensor2MPI_NCCLBase {

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

  // helper function for computing element-wise negation in GPUs
  val CUDA_NEGATE_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_NEGATE_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_NEGATE_KERNEL_MAP.getOrElseUpdate(m, CUDA_NEGATE_KERNEL(m))
  def gpu_neg_array(size: Int, m: Manifest[_], device: INT, operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
	withComment(s"computing NEG on GPU for size $size and type $m at device (pre-rename) ${device.x} with operand $operand") {
		val array = gpu_array(size, m, device)
		val neg_fun = CUDA_NEGATE_FUN(m)
		neg_fun(new ARRAY(operand), array, size, DIM3(gridSize), DIM3(blockSize))
		array
  }

  val unaryOps = List("tensor_negate")

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, op, Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(operand:Backend.Sym)::_, _)
      if unaryOps.contains(op) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // Load the operand, and maybe add communication ops to resolve split annotation conflicts
      val loaded_operand = get_operand(operand, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          op match {
            case "tensor_negate" => gpu_neg_array(count, m, myNCCLRank, loaded_operand).x
            case _ => throw new Exception(s"op $op is not unary op")
          }
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }
    case _ => super.transform(n)
  }
}