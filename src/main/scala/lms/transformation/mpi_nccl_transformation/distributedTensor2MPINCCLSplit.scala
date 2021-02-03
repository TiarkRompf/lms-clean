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


trait DistributeTensor2MPI_NCCLSplit extends DistributeTensor2MPI_NCCLBase {

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

  // helper function for computing split op in GPUs
  val CUDA_SPLIT2_2D1_EQUAL_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_SPLIT2_2D1_EQUAL_FUN(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_SPLIT2_2D1_EQUAL_KERNEL_MAP.getOrElseUpdate(m, CUDA_SPLIT2_2D1_EQUAL_KERNEL(m, "kernel for split2"))
  // We assume that the input is 2D with size (dim0 by 2xdim1)
  // the split is on axis = 1 and each output is of size (dim0 by dim1)
  def gpu_split2_2d1_equal_array(dim0: Int, dim1: Int, m: Manifest[_], device: INT, input: Backend.Exp)(implicit __pos: SourceContext): List[ARRAY] =
    withComment(s"computing Split on GPU for size $dim0 x ${2 * dim1} and type $m at device (pre-rename) ${device.x} with input $input") {
      val array0 = gpu_array(dim0 * dim1, m, device)
      val array1 = gpu_array(dim0 * dim1, m, device)
      val split_fun = CUDA_SPLIT2_2D1_EQUAL_FUN(m)
      split_fun(new ARRAY(input), array0, array1, dim0, dim1, DIM3(gridSize), DIM3(blockSize))
      List(array0, array1)
    }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "op_split", Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
      val oldSplitOp = new SPLIT_OP(s, useOldMetadata = true)
      implicit val sc_ : SourceContext = oldSplitOp.p
      val m = oldSplitOp.getResult(0).et

      require(tts.length == 2)
      require(tts(0).shape.length == 2)
      require(oldSplitOp.axis == 1)
      require(tts(0).shape(1).size == tts(0).shape(1).size)

      val operand = get_operand(input, anno)
      // then we should run this split op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in mult op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tts(0).contains(dim) && dim != oldSplitOp.axis =>
          val shape = tts(0).shapeSizeAfterSplit(dim, devices.size)
          val arrays = gpu_split2_2d1_equal_array(shape(0), shape(1), m, myNCCLRank, operand)
          (oldSplitOp.getResults zip arrays) foreach { case (tensor, array) => subst(tensor.x.asInstanceOf[Backend.Sym]) = array.x }
          Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
      }

    case _ => super.transform(n)
  }
}
