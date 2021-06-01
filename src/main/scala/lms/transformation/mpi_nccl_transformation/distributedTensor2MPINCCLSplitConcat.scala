package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps, CudaLibs}
import lms.transformation.util.DataStructure

import Backend._


trait DistributeTensor2MPI_NCCLSplitConcat extends DistributeTensor2MPI_NCCLBase with CudaLibs {

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

  val CUDA_CONCAT2_2D1_EQUAL_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_CONCAT2_2D1_EQUAL_FUN(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_CONCAT2_2D1_EQUAL_KERNEL_MAP.getOrElseUpdate(m, CUDA_CONCAT2_2D1_EQUAL_KERNEL(m, "kernel for concat2"))
  // We assume that the input is 2D and concat axis = 1 and all inputs are of the same size
  def gpu_concat2_2d1_equal_array(dim0: Int, dim1: Int, m: Manifest[_], device: INT, input0: Backend.Exp, input1: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing Concat on GPU for size $dim0 x ${2 * dim1} and type $m at device (pre-rename) ${device.x} with input0 $input0 input1 $input1") {
      val array = gpu_array(dim0 * 2 * dim1, m, device)
      val concat_fun = CUDA_CONCAT2_2D1_EQUAL_FUN(m)
      concat_fun(new ARRAY(input0), new ARRAY(input1), array, dim0, dim1, DIM3(gridSize), DIM3(blockSize))
      array
    }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensors_split", Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::(input:Backend.Sym)::Backend.Const(axis:Int)::_, _) =>
      val in_dims = tts(0).shape.length   // dimension of input tensor
      val oldSplitOp = new TENSORS(s, useOldMetadata = true)
      implicit val sc_ : SourceContext = oldSplitOp.p
      val m = (new TENSOR(input, useOldMetadata = true)).et

      require(axis == in_dims - 1, "split axis must be the last dimension of input tensor")

      val operand = get_operand(input, anno) // input tensor

      if (in_dims == 2) {
        require(tts.length == 2, "2D split can only handle split section of 2")
        require(tts(0).shape(1).size == tts(1).shape(1).size, "2D split can only handle equal-sized split")

        // then we should run this split op in all devices in the `anno`
        // FIXME(feiw) for now, let's assume that `anno` is for all devices
        anno match {
          case NAnno => throw new Exception(s"TODO: not yet handling NAnno in mult op")
          case SAnno(dim: Dim, devices: Seq[Device], _) if tts(0).contains(dim) && dim != axis =>
            val shape = tts(0).shapeSizeAfterSplit(dim, devices.size)
            val arrays = gpu_split2_2d1_equal_array(shape(0), shape(1), m, myNCCLRank, operand)
            TENSORS.tupleView(arrays.map(_.x))
          case SAnno(dim: Dim, devices: Seq[Device], _) =>
            throw new Exception(s"TODO: not yet handling SAnno with AllReduce in split")
          case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
        }
      } else if (in_dims == 3) {
        val input_shape = tensor_shape(input, useOldMetadata = true)
        val num_outputs = tts.length

        val dimXs = tts map { _.shape(2).size }
        val dimY = tts(0).shape(1).size
        val dimZ = tts(0).shape(0).size
        val output_sizes = dimXs map { _ * dimY * dimZ }
        val outputs = output_sizes map { sz =>
          gpu_array(sz, manifest[Float], myNCCLRank)
        }

        val in_sz = dimZ * dimY * dimXs.reduce(_ + _)
        cuda3DSplitWrap[Float](
          Wrap[Array[Float]](operand),
          outputs map { t => Wrap[Array[Float]](t.x) },
          dimZ, 
          dimY, 
          dimXs,
          dim3(unit[Int]((in_sz + 511)/512)), 
          dim3(unit[Int](512))
        )
        TENSORS.tupleView(outputs.map(_.x))
      } else {
        throw new Exception(s"TODO: higher-dimension tensors are not yet handled by split")
      }

    case Node(s, "tensor_concat", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::Backend.Const(axis:Int)::(inputs:List[Backend.Sym]), _) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      val input_tensors = inputs.map(x => new TENSOR(x, useOldMetadata=true))
      val in_dims = input_tensors(0).shapeSize.length
      require(in_dims - 1 == axis, "concat axis must be the last dimension of input tensor")

      // load the inputs
      val input_operands = inputs.map(get_operand(_, anno))

      if (in_dims == 2) {
        require(inputs.length == 2)
        require(input_tensors(0).shapeSize(1) == input_tensors(1).shapeSize(1))

        anno match {
          case NAnno => throw new Exception(s"TODO: not yet handling NAnno in concat op")
          case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
            val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
            gpu_concat2_2d1_equal_array(input_tensors(0).shapeSize(0), input_tensors(0).shapeSize(1),
              m, myNCCLRank, input_operands(0), input_operands(1)).x
          case SAnno(dim: Dim, devices: Seq[Device], _) =>
            System.out.println(s"tt $tt dim $dim")
            throw new Exception(s"TODO: not yet handling SAnno with AllReduce in concat")
          case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_concat")
        }
      } else if (in_dims == 3) {
        val input_shapes = inputs.map(tensor_shape(_, useOldMetadata = true))

        val dimXs = input_shapes map { _(2) }
        val dimY = input_shapes(0)(1)
        val dimZ = input_shapes(0)(0)
        val out_sz = dimXs.reduce(_ + _) * dimY * dimZ

        val output = gpu_array(out_sz, manifest[Float], myNCCLRank)

        cuda3DConcatWrap[Float](
          input_operands map { t =>  Wrap[Array[Float]](t) },
          Wrap[Array[Float]](output.x),
          dimZ, 
          dimY, 
          dimXs,
          dim3(unit[Int]((out_sz + 511)/512)), 
          dim3(unit[Int](512))
        )
        output.x

      } else {
        throw new Exception(s"TODO: higher-dimension tensors are not yet handled by concat")
      }

    case _ => super.transform(n)
  }
}
