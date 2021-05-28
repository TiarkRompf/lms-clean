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
    
    case Node(s, "tensor_logsoftmax", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // get input info and transform input tensors
      val input_shape = tensor_shape(input, useOldMetadata = true)
      val input_tensor = get_operand(input, anno)

      val outer_size = input_shape.init.fold(1) { _ * _ }
      val last_dim_size = input_shape.last
      val input_size = outer_size * last_dim_size

      val output = gpu_array(input_size, manifest[Float], myNCCLRank)

      val softmaxKernel = cudaSoftmax[Float](true)

      FunOps6(softmaxKernel).apply(
        Wrap[Array[Float]](input_tensor), 
        Wrap[Array[Float]](output.x),
        unit[Int](last_dim_size),
        dim3(unit[Int](outer_size)),
        dim3(unit[Int](1024)),
        unit[Int](1024 * 4)
      )

      output.x
    
    case Node(s, "tensor_logsoftmax_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(output:Backend.Sym)::(doutput:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // get input info and transform input tensors
      val output_shape = tensor_shape(output, useOldMetadata = true)
      val output_tensor = get_operand(output, anno)
      val doutput_tensor = get_operand(doutput, anno)

      val outer_size = output_shape.init.fold(1) { _ * _ }
      val last_dim_size = output_shape.last
      val input_size = outer_size * last_dim_size

      val dinput = gpu_array(input_size, manifest[Float], myNCCLRank)

      val softmaxGradKernel = cudaSoftmaxGrad[Float](true) /* Default is taking log */

      FunOps7(softmaxGradKernel).apply(
        Wrap[Array[Float]](dinput.x), 
        Wrap[Array[Float]](doutput_tensor),
        Wrap[Array[Float]](output_tensor),
        unit[Int](last_dim_size),
        dim3(unit[Int](outer_size)),
        dim3(unit[Int](1024)),
        unit[Int](1024 * 4)
      )

      dinput.x
    
    case Node(s, "tensor_transpose", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(operand:Backend.Sym)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      val input_shape = tensor_shape(operand, useOldMetadata = true)
      val input_tensor = get_operand(operand, anno)

      val n_rows = input_shape(0) / 2 // Luke: HARDCODE!
      val n_cols = input_shape(1)
      
      val size = input_shape.fold(1) { _ * _ }
      val output = gpu_array(size, manifest[Float], myNCCLRank)

      val tileDim = 32
      val blockRows = 8
      
      val transposeKernel = cudaTranspose[Float]
      FunOps6(transposeKernel).apply(
        Wrap[Array[Float]](input_tensor),
        Wrap[Array[Float]](output.x),
        unit[Int](n_rows),
        unit[Int](n_cols),
        dim3(unit[Int]((n_cols + tileDim - 1) / tileDim), unit[Int]((n_rows + tileDim - 1) / tileDim)),
        dim3(unit[Int](tileDim), unit[Int](blockRows)))
      
      output.x


    case Node(s, "tensors_split2", Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::(input:Backend.Sym)::Backend.Const(axis:Int)::_, _) =>
      val oldSplitOp = new TENSORS(s, useOldMetadata = true)
      implicit val sc_ : SourceContext = oldSplitOp.p
      val m = (new TENSOR(input, useOldMetadata = true)).et

      val inuput_tensor = get_operand(input, anno)
      val input_shape = tensor_shape(input, useOldMetadata = true)

      val num_outputs = tts.length
      require(tts(0).shape.length == 3)
      require(axis == 2)
      // require(tts.fold(0) { (acc, i) => acc + i.shape(2).size } == input_shape(2).size)
      
      val dimXs = tts map { _.shape(2).size }
      val dimY = tts(0).shape(1).size
      val dimZ = tts(0).shape(0).size
      val output_sizes = dimXs map { _ * dimY * dimZ }
      val outputs = output_sizes map {sz =>
        gpu_array(sz, manifest[Float], myNCCLRank)
      }
      // TODO: dim checks

      /*
      cuda3DSplitWrap[Float](
        Wrap[Array[Float]](input_tensor),
        outputs,
        unit[Int](dimZ), 
        unit[Int](dimY), 
        dimXs,
        dim3((in_sz + 511)/512), 
        dim3(512)
      )*/
      
      TENSORS.tupleView(outputs.map(_.x))


    case _ => super.transform(n)
  }


}