package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.ListBuffer

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps,CUDNNTypeLess,CLibTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps, CudaLibs}
import lms.transformation.util.{DataStructure, CudnnUtils}

import Backend._


trait DistributeTensor2MPI_NCCLKernels extends DistributeTensor2MPI_NCCLBase with CudnnUtils with CudaLibs {

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
  import CUDNNTypeLess._
  import CLibTypeLess._

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_maskedfill", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::(mask:Backend.Sym)::
      Backend.Const(value:Float)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // get input info and transform input tensors
      val input_shape = tensor_shape(input, useOldMetadata = true)
      val input_tensor = get_operand(input, anno)
      val mask_tensor = get_operand(mask, anno)

      // choose the last two dimensions as dim0 and dim1
      val dim0_shape = input_shape(input_shape.size - 2)
      val dim1_shape = input_shape(input_shape.size - 1)
      val dim0_stride = dim0_shape
      val dim1_stride = 1

      val input_size = input_shape.fold(1) { _ * _ }
      val output = gpu_array(input_size, manifest[Float], myNCCLRank)

      val maskedFillKernel = cudaMaskedFill[Float](false)
      FunOps11(maskedFillKernel).apply(
        Wrap[Array[Float]](input_tensor), 
        Wrap[Array[Float]](output.x),
        Wrap[Array[Int]](mask_tensor),
        unit[Float](value),
        unit[Int](dim0_shape),
        unit[Int](dim1_shape),
        unit[Int](dim0_stride),
        unit[Int](dim1_stride),
        unit[Int](input_size),
        dim3(unit[Int]((input_size + 511)/512)),
        dim3(unit[Int](512))
      )

      output.x

    case Node(s, "tensor_maskedfill_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(doutput:Backend.Sym)::(mask:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // get input info and transform input tensors
      val doutput_shape = tensor_shape(doutput, useOldMetadata = true)
      val doutput_tensor = get_operand(doutput, anno)
      val mask_tensor = get_operand(mask, anno)

      // choose the last two dimensions as dim0 and dim1
      val dim0_shape = doutput_shape(doutput_shape.size - 2)
      val dim1_shape = doutput_shape(doutput_shape.size - 1)
      val dim0_stride = dim0_shape
      val dim1_stride = 1

      val doutput_size = doutput_shape.fold(1) { _ * _ }
      val dinput = gpu_array(doutput_size, manifest[Float], myNCCLRank)

      val maskedFillGradKernel = cudaMaskedFillGrad[Float](false)
      FunOps10(maskedFillGradKernel).apply(
        Wrap[Array[Float]](doutput_tensor), 
        Wrap[Array[Float]](dinput.x),
        Wrap[Array[Int]](mask_tensor),
        unit[Int](dim0_shape),
        unit[Int](dim1_shape),
        unit[Int](dim0_stride),
        unit[Int](dim1_stride),
        unit[Int](doutput_size),
        dim3(unit[Int]((doutput_size + 511)/512)),
        dim3(unit[Int](512))
      )

      dinput.x

    case _ => super.transform(n)
  }


}