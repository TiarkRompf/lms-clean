package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

trait CUDNNBaseTypeLess extends Dsl with CLibs {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class CUDNN_HANDLE(override val x: Backend.Exp) extends TOP(x)

  var cache: Option[CUDNN_HANDLE] = None
  def CUDNN_HANDLE(implicit __pos: SourceContext) = cache match {
    case Some(x) => x
    case None =>
      val handle = new CUDNN_HANDLE(NEW_STRUCT(manifest[CUDNN_HANDLE], "cudnnHandle_t").x)
      cache = Some(handle)
      handle
  }

  class CUDNN_RESULT

  def CUDNN_CHECK(result: TOP)(implicit __pos: SourceContext): UNIT = {
    assert(result.t == manifest[CUDNN_RESULT], "CUDNN_CHECK must take the CUDNN_RESULT type as input")
    UNIT(LIB_FUNCTION(manifest[Unit], "CUDNNCHECK", result.x)(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL))
  }

  def CUDNN_NCHW(implicit __pos: SourceContext): INT = INT(CMACRO("CUDNN_TENSOR_NCHW", manifest[Int]))
  def CUDNN_NHWC(implicit __pos: SourceContext): INT = INT(CMACRO("CUDNN_TENSOR_NHWC", manifest[Int]))
  def CUDNN_FLOAT(implicit __pos: SourceContext): INT = INT(CMACRO("CUDNN_DATA_FLOAT", manifest[Int]))

  def CUDNN_PROPAGATE_NAN(implicit __pos: SourceContext): INT = INT(CMACRO("CUDNN_PROPAGATE_NAN", manifest[Int]))
  def CUDNN_NOT_PROPAGATE_NAN(implicit __pos: SourceContext): INT = INT(CMACRO("CUDNN_NOT_PROPAGATE_NAN", manifest[Int]))

  class CUDNN_TENSOR_DESCRIPTOR(override val x: Backend.Exp) extends TOP(x)
  class CUDNN_FILTER_DESCRIPTOR(override val x: Backend.Exp) extends TOP(x)

  def CUDNN_CREATE_TENSOR_DESCRIPTOR(tensorDesc: CUDNN_TENSOR_DESCRIPTOR)(implicit __pos: SourceContext) = 
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnCreateTensorDescriptor", tensorDesc.x)(Seq(), Seq(0), Set[Int](0))

  def CUDNN_DESTROY_TENSOR_DESCRIPTOR(tensorDesc: TOP)(implicit __pos: SourceContext) = 
  LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDestroyTensorDescriptor", tensorDesc.x)(Seq(), Seq(0), Set[Int]())
  
  def CUDNN_SET_TENSOR_4D_DESCRIPTOR(tensorDesc: CUDNN_TENSOR_DESCRIPTOR, format: TOP, dataType: TOP, n: INT, c: INT, 
                                     h: INT, w: INT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSetTensor4dDescriptor", tensorDesc.x, format.x, dataType.x,
      n.x, c.x, h.x, w.x)(Seq(1,2,3,4,5,6), Seq(0), Set[Int]())
  
  def CUDNN_CREATE_FILTER_DESCRIPTOR(filterDesc: CUDNN_FILTER_DESCRIPTOR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnCreateFilterDescriptor", filterDesc.x)(Seq(), Seq(0), Set[Int](0))
  
  def CUDNN_DESTROY_FILTER_DESCRIPTOR(filterDesc: TOP)(implicit __pos: SourceContext) = 
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDestroyFilterDescriptor", filterDesc.x)(Seq(), Seq(0), Set[Int]())
  
  def CUDNN_SET_FILTER_4D_DESCRIPTOR(filterDesc: CUDNN_FILTER_DESCRIPTOR, format: TOP, dataType: TOP, k: INT, c: INT,
                                     h: INT, w: INT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSetFilter4dDescriptor", filterDesc.x, format.x, dataType.x,
      k.x, c.x, h.x, w.x)(Seq(1,2,3,4,5,6), Seq(0), Set[Int]())

}

trait CUDNNBaseOps extends CLibs with CudaOps {
  /* LMS support for CUDNN library */

  class cudnnStatusT

  abstract class cudnnHandleT
  // def cudnnHandle: Rep[cudnnHandleT] = newStruct[cudnnHandleT]("cudnnHandle_t")
  lazy val cudnnHandle = newStruct[cudnnHandleT]("cudnnHandle_t")

  class cudnnTensorDescriptorT
  def cudnnTensorDescriptor: Rep[cudnnTensorDescriptorT] = newStruct[cudnnTensorDescriptorT]("cudnnTensorDescriptor_t")
  class cudnnFilterDescriptorT
  def cudnnFilterDescriptor: Rep[cudnnFilterDescriptorT] = newStruct[cudnnFilterDescriptorT]("cudnnFilterDescriptor_t")

  class cudnnTensorFormatT
  def cudnnNCHW = cmacro[cudnnTensorFormatT]("CUDNN_TENSOR_NCHW")
  def cudnnNHWC = cmacro[cudnnTensorFormatT]("CUDNN_TENSOR_NHWC")
  def cudnnNCHW_VECT_C = cmacro[cudnnTensorFormatT]("CUDNN_TENSOR_NCHW_VECT_C")

  class cudnnDataTypeT
  def cudnnFloat = cmacro[cudnnDataTypeT]("CUDNN_DATA_FLOAT")  // 32-bit
  def cudnnDouble = cmacro[cudnnDataTypeT]("CUDNN_DATA_DOUBLE")  // 64-bit
  def cudnnHalf = cmacro[cudnnDataTypeT]("CUDNN_DATA_HALF")  // 16-bit
  def cudnnInt8 = cmacro[cudnnDataTypeT]("CUDNN_DATA_UINT8")
  def cudnnInt32 = cmacro[cudnnDataTypeT]("CUDNN_DATA_INT32")
  def cudnnInt8x4 = cmacro[cudnnDataTypeT]("CUDNN_DATA_INT8x4")
  def cudnnInt8x32 = cmacro[cudnnDataTypeT]("CUDNN_DATA_INT8x32")
  def cudnnInt8x64 = cmacro[cudnnDataTypeT]("CUDNN_DATA_UINT8x4")

  class cudnnNanPropagationT
  def cudnnPropagateNan = cmacro[cudnnNanPropagationT]("CUDNN_PROPAGATE_NAN")
  def cudnnNotPropagateNan = cmacro[cudnnNanPropagationT]("CUDNN_NOT_PROPAGATE_NAN")

  
  def cudnnCheck(res: Rep[cudnnStatusT]) =
    libFunction[Unit]("CUDNNCHECK", Unwrap(res))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  
  // cudnnStatus_t cudnnCreate(cudnnHandle_t *handle)
  def cudnnCreate(handle: Rep[cudnnHandleT]): Rep[cudnnStatusT] =
    libFunction[cudnnStatusT]("cudnnCreate", Unwrap(handle))(Seq(), Seq(0), Set(0))
  
  // cudnnStatus_t cudnnDestroy(cudnnHandle_t handle)
  def cudnnDestroy(handle: Rep[cudnnHandleT]): Rep[cudnnStatusT] =
    libFunction[cudnnStatusT]("cudnnDestroy", Unwrap(handle))(Seq(), Seq(0), Set())


  // cudnnStatus_t cudnnCreateTensorDescriptor(cudnnTensorDescriptor_t *tensorDesc)
  def cudnnCreateTensorDescriptor(tensorDesc: Rep[cudnnTensorDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreateTensorDescriptor", Unwrap(tensorDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyTensorDescriptor(cudnnTensorDescriptor_t tensorDesc)
  def cudnnDestroyTensorDescriptor(tensorDesc: Rep[cudnnTensorDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyTensorDescriptor", Unwrap(tensorDesc))(Seq(), Seq(0), Set())
  
  /*
  cudnnStatus_t cudnnSetTensor4dDescriptor(
                                cudnnTensorDescriptor_t tensorDesc,
                                cudnnTensorFormat_t     format,
                                cudnnDataType_t         dataType,
                                int                     n,          // number of images
                                int                     c,          // number of feature maps per image
                                int                     h,          // height of each feature map
                                int                     w)          // width of each feature map
  */
  def cudnnSetTensor4dDescriptor(tensorDesc: Rep[cudnnTensorDescriptorT], format: Rep[cudnnTensorFormatT],
                                dataType: Rep[cudnnDataTypeT], n: Rep[Int], c: Rep[Int], h: Rep[Int], w: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnSetTensor4dDescriptor", Unwrap(tensorDesc), Unwrap(format), Unwrap(dataType),
      Unwrap(n), Unwrap(c), Unwrap(h), Unwrap(w))(Seq(1,2,3,4,5,6), Seq(0), Set())
  

  // cudnnStatus_t cudnnCreateFilterDescriptor(cudnnFilterDescriptor_t *filterDesc)
  def cudnnCreateFilterDescriptor(filterDesc: Rep[cudnnFilterDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreateFilterDescriptor", Unwrap(filterDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyFilterDescriptor(cudnnFilterDescriptor_t filterDesc)
  def cudnnDestroyFilterDescriptor(filterDesc: Rep[cudnnFilterDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyFilterDescriptor", Unwrap(filterDesc))(Seq(), Seq(0), Set())

  /*
  cudnnStatus_t cudnnSetFilter4dDescriptor(
                                cudnnFilterDescriptor_t filterDesc,
                                cudnnTensorFormat_t     format,
                                cudnnDataType_t         dataType,
                                int                     k,          // number of output feature maps
                                int                     c,          // number of input feature maps
                                int                     h,          // height of each feature map
                                int                     w)          // width of each feature map
  */
  def cudnnSetFilter4dDescriptor(filterDesc: Rep[cudnnFilterDescriptorT], format: Rep[cudnnTensorFormatT], dataType: Rep[cudnnDataTypeT],
                                 k: Rep[Int], c: Rep[Int], h: Rep[Int], w: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnSetFilter4dDescriptor", Unwrap(filterDesc), Unwrap(dataType), Unwrap(format),
      Unwrap(k), Unwrap(c), Unwrap(h), Unwrap(w))(Seq(1,2,3,4,5,6), Seq(0), Set())
  
}

trait CCodeGenCUDNNBase extends ExtendedCCodeGen {
  override def remap(m: Manifest[_]) = m.runtimeClass.getName match {
    case s: String if s.endsWith("$cudnn_result") ||
      s.endsWith("$CUDNN_RESULT")=> "cudnnStatus_t"
    
    case s => super.remap(m)
  }
}