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


trait DistributeTensor2MPI_NCCLBinary extends DistributeTensor2MPI_NCCLBase {

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

  // helper function for computing element-wise addition in GPUs
  val CUDA_ADD_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_ADD_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_ADD_KERNEL_MAP.getOrElseUpdate(m, CUDA_ADD_KERNEL(m))
  def gpu_add_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing ADD on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val add_fun = CUDA_ADD_FUN(m)
      add_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for computing element-wise subtraction in GPUs
  val CUDA_MINUS_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_MINUS_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_MINUS_KERNEL_MAP.getOrElseUpdate(m, CUDA_MINUS_KERNEL(m))
  def gpu_sub_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing SUB on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val sub_fun = CUDA_MINUS_FUN(m)
      sub_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for computing element-wise multiplication in GPUs
  val CUDA_MULT_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_MULT_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_MULT_KERNEL_MAP.getOrElseUpdate(m, CUDA_MULT_KERNEL(m))
  def gpu_mult_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing MULT on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val mult_fun = CUDA_MULT_FUN(m)
      mult_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for computing element-wise division in GPUs
  val CUDA_DIV_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_DIV_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_DIV_KERNEL_MAP.getOrElseUpdate(m, CUDA_DIV_KERNEL(m))
  def gpu_div_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing DIV on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val div_fun = CUDA_DIV_FUN(m)
      div_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for computing element-wise tanh gradient in GPUs
  val CUDA_TANH_GRAD_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_TANH_GRAD_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_TANH_GRAD_KERNEL_MAP.getOrElseUpdate(m, CUDA_TANH_GRAD_KERNEL(m))
  def gpu_tanh_grad_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing TANH_GRAD on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val tanh_grad_fun = CUDA_TANH_GRAD_FUN(m)
      tanh_grad_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  
  // helper function for computing element-wise relu gradient in GPUs
  val CUDA_RELU_GRAD_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_RELU_GRAD_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_RELU_GRAD_KERNEL_MAP.getOrElseUpdate(m, CUDA_RELU_GRAD_KERNEL(m))
  def gpu_relu_grad_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing RELU_GRAD on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val relu_grad_fun = CUDA_RELU_GRAD_FUN(m)
      relu_grad_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }

  // helper function for computing element-wise invert gradient in GPUs
  val CUDA_INVERT_GRAD_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_INVERT_GRAD_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_INVERT_GRAD_KERNEL_MAP.getOrElseUpdate(m, CUDA_INVERT_GRAD_KERNEL(m))
  def gpu_invert_grad_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing INVERT_GRAD on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val invert_grad_fun = CUDA_INVERT_GRAD_FUN(m)
      invert_grad_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }

  val binaryOps = List("tensor_add", "tensor_sub", "tensor_mult", "tensor_div", "tensor_tanh_grad", "tensor_relu_grad", "tensor_invert_grad")

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, op, Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _)
      if binaryOps.contains(op) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      // then we should run this mult op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in mult op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          op match {
            case "tensor_add" => gpu_add_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case "tensor_sub" => gpu_sub_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case "tensor_mult" => gpu_mult_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case "tensor_div" => gpu_div_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case "tensor_tanh_grad" => gpu_tanh_grad_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case "tensor_relu_grad" => gpu_relu_grad_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case "tensor_invert_grad" => gpu_invert_grad_array(count2, m, myNCCLRank, left_operand, right_operand).x
            case _ => throw new Exception(s"op $op is not binary op")
          }
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
      }

    case _ => super.transform(n)
  }
}
