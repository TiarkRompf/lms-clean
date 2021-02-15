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


trait DistributeTensor2MPI_NCCLMutation extends DistributeTensor2MPI_NCCLBase {

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

  // helper function for accumulate (+=) a GPU array element-wise
  val CUDA_ACCUM_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_ACCUM_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_ACCUM_KERNEL_MAP.getOrElseUpdate(m, CUDA_ACCUM_KERNEL(m))
  def gpu_accum_array(size: Int, m: Manifest[_], device: INT, base_operand: Backend.Exp, addition_operand: Backend.Exp)(implicit __pos: SourceContext) =
    withComment(s"computing ACCUM on GPU for size $size and type $m at device (pre-rename) ${device.x} with base_operand $base_operand and addition_operand $addition_operand") {
      CUDA_SET_DEVICE(device)
      val accum_fun = CUDA_ACCUM_FUN(m)
      accum_fun(new ARRAY(base_operand), new ARRAY(addition_operand), size, DIM3(gridSize), DIM3(blockSize))
      Backend.Const(())
    }
  // helper function for SGD optimization on GPU array
  val CUDA_SGD_Nesterov_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_SGD_Nesterov_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_SGD_Nesterov_KERNEL_MAP.getOrElseUpdate(m, CUDA_SGD_Nesterov_KERNEL(m))
  def gpu_sgd_array(size: Int, m: Manifest[_], device: INT, weight_operand: Backend.Exp, grad_operand: Backend.Exp, momentum_operand: Backend.Exp)(implicit __pos: SourceContext) =
    withComment(s"computing SGD on GPU for size $size and type $m at device (pre-name) ${device.x} with weight $weight_operand, grad $grad_operand, and momentum $momentum_operand") {
      CUDA_SET_DEVICE(device)
      val sgd_fun = CUDA_SGD_Nesterov_FUN(m)
      sgd_fun(new ARRAY(weight_operand), new ARRAY(grad_operand), new ARRAY(momentum_operand), size, DIM3(gridSize), DIM3(blockSize))
      Backend.Const(())
    }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "accum_tensor", Backend.Const(anno:Anno)::(base:Backend.Exp)::(addition:Backend.Exp)::_, _) =>
      val sourceTensor = new TENSOR(base, useOldMetadata = true)

      implicit val pos = Adapter.oldSourceMap(s)
      val tt = sourceTensor.resultType
      val m = sourceTensor.et

      // Load the `base` and `addition`, and maybe add communication ops to resolve split annotation conflicts
      val base_operand = get_operand(base, anno, assertSame=true)
      val addition_operand = get_operand(addition, anno)
      // then we should run this accumulation op in all devices in the `anno`
      // FIXME(feiw) for now, let's assum that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in accum op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_accum_array(count2, m, myNCCLRank, base_operand, addition_operand)
          Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(sourceTensor.shapeSize)
          gpu_accum_array(count, m, myNCCLRank, base_operand, addition_operand)
          Backend.Const(())
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in accum_tensor")
      }

    case Node(s, "optimize_tensor", Backend.Const(anno:Anno)::(weight:Backend.Exp)::(grad:Backend.Exp)::(momentum:Backend.Exp)::_, _) =>
      val sourceTensor = new TENSOR(weight, useOldMetadata = true)

      implicit val pos = Adapter.oldSourceMap(s)
      val tt = sourceTensor.resultType
      val m = sourceTensor.et
      val anno = sourceTensor.annotation

      // Load the `weight` `grad` `momentum`. Only `grad` might need extra communication ops to resolve split annotation conflicts
      val weight_operand = get_operand(weight, anno, assertSame=true)
      val grad_operand = get_operand(grad, anno)
      val momentum_operand = get_operand(momentum, anno, assertSame=true)
      // then we should run this optimization op in all devices in the `anno`
      // FIXME(feiw) for now, let's assum that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in optimize_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_sgd_array(count2, m, myNCCLRank, weight_operand, grad_operand, momentum_operand)
          Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(sourceTensor.shapeSize)
          gpu_sgd_array(count, m, myNCCLRank, weight_operand, grad_operand, momentum_operand)
          Backend.Const(())
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in optimize_tensor")
      }

    case _ => super.transform(n)
  }
}
