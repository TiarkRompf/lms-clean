package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

// TODO: change type of alpha and beta from Rep[Array[_]] to Var[Float]

object CUDNNTypeLess extends Dsl with CLibs {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class CUDNN_HANDLE(override val x: Backend.Exp) extends TOP(x)

  var cache: Option[CUDNN_HANDLE] = None
  def CUDNN_HANDLE(implicit __pos: SourceContext) = cache match {
    case Some(x) => x
    case None => 
      val handle = new CUDNN_HANDLE(NEW_STRUCT(manifest[CUDNN_HANDLE]).x)
      cache = Some(handle)
      handle
  }

  class CUDNN_RESULT

  def CUDNN_CHECK(result: TOP)(implicit __pos: SourceContext): UNIT = {
    assert(result.t == manifest[CUDNN_RESULT], "CUDNN_CHECK must take the CUDNN_RESULT type as input")
    UNIT(LIB_FUNCTION(manifest[Unit], "CUDNNCHECK", result.x)(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL))
  }
} 


trait CUDNNOps extends CLibs with CudaOps {

  /* LMS support for CUDNN library */
  
  class cudnnStatusT

  abstract class cudnnHandleT
  // def cudnnHandle: Rep[cudnnHandleT] = newStruct[cudnnHandleT]("cudnnHandle_t")
  lazy val cudnnHandle = newStruct[cudnnHandleT]("cudnnHandle_t")

  class cudnnTensorDescriptorT
  def cudnnTensorDescriptor: Rep[cudnnTensorDescriptorT] = newStruct[cudnnTensorDescriptorT]("cudnnTensorDescriptor_t")

  class cudnnActivationDescriptorT
  def cudnnActivationDescriptor: Rep[cudnnActivationDescriptorT] = newStruct[cudnnActivationDescriptorT]("cudnnActivationDescriptor_t")

  class cudnnAlgorithmDescriptorT
  def cudnnAlgorithmDescriptor: Rep[cudnnAlgorithmDescriptorT] = newStruct[cudnnAlgorithmDescriptorT]("cudnnAlgorithmDescriptor_t")

  class cudnnFilterDescriptorT
  def cudnnFilterDescriptor: Rep[cudnnFilterDescriptorT] = newStruct[cudnnFilterDescriptorT]("cudnnFilterDescriptor_t")

  class cudnnConvolutionDescriptorT
  def cudnnConvolutionDescriptor: Rep[cudnnConvolutionDescriptorT] = newStruct[cudnnConvolutionDescriptorT]("cudnnConvolutionDescriptor_t")

  class cudnnPoolingDescriptorT
  def cudnnPoolingDescriptor: Rep[cudnnPoolingDescriptorT] = newStruct[cudnnPoolingDescriptorT]("cudnnPoolingDescriptor_t")

  class cudnnDropoutDescriptorT
  def cudnnDropoutDescriptor: Rep[cudnnDropoutDescriptorT] = newStruct[cudnnDropoutDescriptorT]("cudnnDropoutDescriptor_t")

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

  class cudnnConvolutionModeT
  def cudnnConvolution = cmacro[cudnnConvolutionModeT]("CUDNN_CONVOLUTION")
  def cudnnCrossCorrelation = cmacro[cudnnConvolutionModeT]("CUDNN_CROSS_CORRELATION")

  class cudnnConvolutionFwdAlgoT
  def cudnnConvolutionFwdAlgo = newStruct[cudnnConvolutionFwdAlgoT]("cudnnConvolutionFwdAlgo_t")
  
  class cudnnConvolutionFwdAlgoPerfT
  def cudnnConvolutionFwdAlgoPerf = newStruct[cudnnConvolutionFwdAlgoPerfT]("cudnnConvolutionFwdAlgoPerf_t")

  class cudnnConvolutionBwdDataAlgoPerfT

  class cudnnSoftmaxAlgorithmT
  def cudnnSoftmaxFast = cmacro[cudnnSoftmaxAlgorithmT]("CUDNN_SOFTMAX_FAST")
  def cudnnSoftmaxAccurate = cmacro[cudnnSoftmaxAlgorithmT]("CUDNN_SOFTMAX_ACCURATE")
  def cudnnSoftmaxLog = cmacro[cudnnSoftmaxAlgorithmT]("CUDNN_SOFTMAX_LOG")

  class cudnnSoftmaxModeT
  def cudnnSoftmaxModeInstance = cmacro[cudnnSoftmaxModeT]("CUDNN_SOFTMAX_MODE_INSTANCE")
  def cudnnsoftmaxModeChannel = cmacro[cudnnSoftmaxModeT]("CUDNN_SOFTMAX_MODE_CHANNEL")

  class cudnnActivationModeT
  def cudnnActivationSigmoid = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_SIGMOID")
  def cudnnActivationRelu = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_RELU")
  def cudnnActivationTanh = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_RELU")
  def cudnnActivationClippedRelu = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_CLIPPED_RELU")
  def cudnnActivationElu = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_ELU")
  def cudnnActivationIdentity = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_IDENTITY")

  class cudnnPropagationT
  def cudnnPropagateNan = cmacro[cudnnPropagationT]("CUDNN_PROPAGATE_NAN")
  def cudnnNotPropagateNan = cmacro[cudnnPropagationT]("CUDNN_NOT_PROPAGATE_NAN")

  class cudnnConvolutionBwdFilterAlgoT
  def cudnnConvolutionBwdFilterAlgo0 = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_0")
  def cudnnConvolutionBwdFilterAlgo1 = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_1")
  def cudnnConvolutionBwdFilterAlgoFFT = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_FFT")
  def cudnnConvolutionBwdFilterAlgo3 = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_3")
  def cudnnConvolutionBwdFilterWinogradNonfused = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_WINOGRAD_NONFUSED")
  def cudnnConvolutionBwdFilterAlgoFFTTiling = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_FFT_TILING")

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

  // cudnnStatus_t cudnnCreateActivationDescriptor(cudnnActivationDescriptor_t *activationDesc)
  def cudnnCreateActivationDescriptor(activationDesc: Rep[cudnnActivationDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnCreateActivationDescriptor", Unwrap(activationDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyActivationDescriptor(cudnnActivationDescriptor_t activationDesc)
  def cudnnDestroyActivationDescriptor(activationDesc: Rep[cudnnActivationDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyActivationDescriptor", Unwrap(activationDesc))(Seq(), Seq(0), Set())

  // cudnnStatus_t cudnnCreateAlgorithmDescriptor(cudnnAlgorithmDescriptor_t *algoDesc)
  def cudnnCreateAlgorithmDescriptor(algoDesc: Rep[cudnnAlgorithmDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnCreateAlgorithmDescriptor", Unwrap(algoDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyAlgorithmPerformance(cudnnAlgorithmPerformance_t algoPerf)
  def cudnnDestroyAlgorithmDescriptor(algoDesc: Rep[cudnnAlgorithmDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyAlgorithmDescriptor", Unwrap(algoDesc))(Seq(), Seq(0), Set())

  // cudnnStatus_t cudnnCreateFilterDescriptor(cudnnFilterDescriptor_t *filterDesc)
  def cudnnCreateFilterDescriptor(filterDesc: Rep[cudnnFilterDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnCreateFilterDescriptor", Unwrap(filterDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyFilterDescriptor(cudnnFilterDescriptor_t filterDesc)
  def cudnnDestroyFilterDescriptor(filterDesc: Rep[cudnnFilterDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnDestroyFilterDescriptor", Unwrap(filterDesc))(Seq(), Seq(0), Set())

  // cudnnStatus_t cudnnCreateConvolutionDescriptor(cudnnConvolutionDescriptor_t *convDesc)
  def cudnnCreateConvolutionDescriptor(convDesc: Rep[cudnnConvolutionDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnCreateConvolutionDescriptor", Unwrap(convDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyConvolutionDescriptor(cudnnConvolutionDescriptor_t convDesc)
  def cudnnDestroyConvolutionDescriptor(convDesc: Rep[cudnnConvolutionDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnDestroyConvolutionDescriptor", Unwrap(convDesc))(Seq(), Seq(0), Set())

  // cudnnStatus_t cudnnCreatePoolingDescriptor(cudnnPoolingDescriptor_t    *poolingDesc)
  def cudnnCreatePoolingDescriptor(poolingDesc: Rep[cudnnPoolingDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnCreateFilterDescriptor", Unwrap(poolingDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyPoolingDescriptor(cudnnConvolutionDescriptor_t poolingDesc)
  def cudnnDestroyPoolingDescriptor(poolingDesc: Rep[cudnnPoolingDescriptorT]) = 
    libFunction[cudnnStatusT]("cudnnDestroyPoolingDescriptor", Unwrap(poolingDesc))(Seq(), Seq(0), Set())

  // cudnnStatus_t cudnnCreateDropoutDescriptor(cudnnDropoutDescriptor_t    *dropoutDesc)
  def cudnnCreateDropoutDescriptor(dropoutDesc: Rep[cudnnDropoutDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreateDropoutDescriptor", Unwrap(dropoutDesc))(Seq(), Seq(0), Set(0))
  
  // cudnnStatus_t cudnnStatus_t cudnnDestroyDropoutDescriptor(cudnnDropoutDescriptor_t dropoutDesc)
  def cudnnDestroyDropoutDescriptor(dropoutDesc: Rep[cudnnDropoutDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyDropoutDescriptor", Unwrap(dropoutDesc))(Seq(), Seq(0), Set())

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

  /*
  cudnnStatus_t cudnnSetConvolution2dDescriptor(
                                cudnnConvolutionDescriptor_t    convDesc,
                                int                             pad_h,
                                int                             pad_w,
                                int                             u,            // vertical filter stride
                                int                             v,            // horizontal filter stride
                                int                             dilation_h,   // filter height dilation
                                int                             dilation_w,   // filter width dilation
                                cudnnConvolutionMode_t          mode,
                                cudnnDataType_t                 computeType)
  */
  def cudnnSetConvolution2dDescriptor(convDesc: Rep[cudnnConvolutionDescriptorT], pad_h: Rep[Int], pad_w: Rep[Int], u: Rep[Int], v: Rep[Int], 
                                      dilation_h: Rep[Int], dilation_w: Rep[Int], mode: Rep[cudnnConvolutionModeT], computeType: Rep[cudnnDataTypeT]) =
    libFunction[cudnnStatusT]("cudnnSetConvolution2dDescriptor", Unwrap(convDesc), Unwrap(pad_h), Unwrap(pad_w), 
      Unwrap(u), Unwrap(v), Unwrap(dilation_h), Unwrap(dilation_w), Unwrap(mode), Unwrap(computeType))(Seq(1,2,3,4,5,6,7,8), Seq(0), Set())

  /*
  cudnnStatus_t cudnnFindConvolutionForwardAlgorithm(
                                cudnnHandle_t                      handle,
                                const cudnnTensorDescriptor_t      xDesc,                 // input tensor descriptor
                                const cudnnFilterDescriptor_t      wDesc,                 // filter descriptor
                                const cudnnConvolutionDescriptor_t convDesc,              // convolution descriptor
                                const cudnnTensorDescriptor_t      yDesc,                 // output tensor descriptor
                                const int                          requestedAlgoCount,    // maximum number of elements to be stored in perfResults
                                int                               *returnedAlgoCount,     // number of output elements stored in prefResults
                                cudnnConvolutionFwdAlgoPerf_t     *perfResults)           // a user-allocated array to store performance metrics 
                                                                                          // sorted ascending by compute time
  */
  def cudnnFindConvolutionForwardAlgorithm(handle: Rep[cudnnHandleT], xDesc: Rep[cudnnTensorDescriptorT], wDesc: Rep[cudnnFilterDescriptorT],
                                           convDesc: Rep[cudnnConvolutionDescriptorT], yDesc: Rep[cudnnTensorDescriptorT], requestedAlgoCount: Rep[Int], 
                                           returnedAlgoCount: Var[Int], perfResults: Rep[cudnnConvolutionFwdAlgoPerfT]) = 
    libFunction[cudnnStatusT]("cudnnFindConvolutionForwardAlgorithm", Unwrap(handle), Unwrap(xDesc), Unwrap(wDesc), Unwrap(convDesc),
      Unwrap(yDesc), Unwrap(requestedAlgoCount), UnwrapV(returnedAlgoCount), Unwrap(perfResults))(Seq(0,1,2,3,4,5,7), Seq(6,7), Set(6,7))


  /*
  cudnnStatus_t cudnnGetConvolutionForwardWorkspaceSize(
                                cudnnHandle_t   handle,
                                const   cudnnTensorDescriptor_t         xDesc,
                                const   cudnnFilterDescriptor_t         wDesc,
                                const   cudnnConvolutionDescriptor_t    convDesc,
                                const   cudnnTensorDescriptor_t         yDesc,
                                cudnnConvolutionFwdAlgo_t               algo,
                                size_t                                 *sizeInBytes)
  */
  def cudnnGetConvolutionForwardWorkspaceSize(handle: Rep[cudnnHandleT], xDesc: Rep[cudnnTensorDescriptorT], wDesc: Rep[cudnnFilterDescriptorT],
                                              convDesc: Rep[cudnnConvolutionDescriptorT], yDesc: Rep[cudnnTensorDescriptorT], algo: Rep[cudnnConvolutionFwdAlgoT], 
                                              sizeInBytes: Var[SizeT]) = // or sizeT?
    libFunction[cudnnStatusT]("cudnnGetConvolutionForwardWorkspaceSize", Unwrap(handle), Unwrap(xDesc), Unwrap(wDesc), Unwrap(convDesc), Unwrap(yDesc),
      Unwrap(algo), UnwrapV(sizeInBytes))(Seq(0,1,2,3,4,5), Seq(6), Set(6))

  /*
  cudnnStatus_t cudnnGetConvolution2dForwardOutputDim(
                                const cudnnConvolutionDescriptor_t  convDesc,
                                const cudnnTensorDescriptor_t       inputTensorDesc,
                                const cudnnFilterDescriptor_t       filterDesc,
                                int                                *n,
                                int                                *c,
                                int                                *h,
                                int                                *w)
  */
  def cudnnGetConvolution2dForwardOutputDim(convDesc: Rep[cudnnConvolutionDescriptorT], inputTensorDesc: Rep[cudnnTensorDescriptorT], filterDesc: Rep[cudnnFilterDescriptorT],
                                n: Var[Int], c: Var[Int], h: Var[Int], w: Var[Int]) =
    libFunction[cudnnStatusT]("cudnnGetConvolution2dForwardOutputDim", Unwrap(convDesc), Unwrap(inputTensorDesc), Unwrap(filterDesc), UnwrapV(n), UnwrapV(c), 
      UnwrapV(h), UnwrapV(w))(Seq(0,1,2), Seq(3,4,5,6), Set(3,4,5,6))

  /*
  cudnnStatus_t cudnnConvolutionForward(
                                cudnnHandle_t                       handle,
                                const void                         *alpha,
                                const cudnnTensorDescriptor_t       xDesc,
                                const void                         *x,
                                const cudnnFilterDescriptor_t       wDesc,
                                const void                         *w,
                                const cudnnConvolutionDescriptor_t  convDesc,
                                cudnnConvolutionFwdAlgo_t           algo,
                                void                               *workSpace,
                                size_t                              workSpaceSizeInBytes,
                                const void                         *beta,
                                const cudnnTensorDescriptor_t       yDesc,
                                void                               *y)
  */
  def cudnnConvolutionForward(handle: Rep[cudnnHandleT], alpha: Var[Float], xDesc: Rep[cudnnTensorDescriptorT], x: Rep[Array[_]], 
                              wDesc: Rep[cudnnFilterDescriptorT], w: Rep[Array[_]], convDesc: Rep[cudnnConvolutionDescriptorT], 
                              algo: Rep[cudnnConvolutionFwdAlgoT], workspace: Rep[Array[_]], workSpaceSizeInBytes: Rep[SizeT], 
                              beta: Var[Float], yDesc: Rep[cudnnTensorDescriptorT], y: Rep[Array[_]]) =
    libFunction[cudnnStatusT]("cudnnConvolutionForward", Unwrap(handle), UnwrapV(alpha), Unwrap(xDesc), Unwrap(x), Unwrap(wDesc), Unwrap(w), Unwrap(convDesc), 
      Unwrap(algo), Unwrap(workspace), Unwrap(workSpaceSizeInBytes), UnwrapV(beta), Unwrap(yDesc), Unwrap(y))(Seq(0,1,2,3,4,5,6,7,8,9,10,11), Seq(1,5,12), 
      Set(1,10))


  /*
  cudnnStatus_t cudnnConvolutionBackwardData(
                                cudnnHandle_t                       handle,
                                const void                         *alpha,
                                const cudnnFilterDescriptor_t       wDesc,
                                const void                         *w,
                                const cudnnTensorDescriptor_t       dyDesc,
                                const void                         *dy,
                                const cudnnConvolutionDescriptor_t  convDesc,
                                cudnnConvolutionBwdDataAlgo_t       algo,
                                void                               *workSpace,
                                size_t                              workSpaceSizeInBytes,
                                const void                         *beta,
                                const cudnnTensorDescriptor_t       dxDesc,
                                void                               *dx)
  */
  def cudnnConvolutionBackwardData(handle: Rep[cudnnHandleT], alpha: Rep[Array[_]], wDesc: Rep[cudnnFilterDescriptorT], w: Rep[Array[_]], 
                                  dyDesc: Rep[cudnnFilterDescriptorT], dy: Rep[Array[_]], convDesc: Rep[cudnnConvolutionDescriptorT], 
                                  algo: Rep[cudnnConvolutionFwdAlgoT], workspace: Rep[Array[_]], workSpaceSizeInBytes: Rep[Int], beta: Rep[Array[_]],
                                  dxDesc: Rep[cudnnTensorDescriptorT], dx: Rep[Array[_]]) =
    libFunction[cudnnStatusT]("cudnnConvolutionBackwardData", Unwrap(handle), Unwrap(alpha), Unwrap(wDesc), Unwrap(w), Unwrap(dyDesc), Unwrap(dy), Unwrap(convDesc),
      Unwrap(algo), Unwrap(workspace), Unwrap(workSpaceSizeInBytes), Unwrap(beta), Unwrap(dxDesc), Unwrap(dx))(Seq(0,1,2,3,4,5,6,7,8,9,10,11), Seq(12), Set(2,9))

  /*
  cudnnStatus_t cudnnConvolutionBackwardFilter(
                                cudnnHandle_t                       handle,
                                const void                         *alpha,
                                const cudnnTensorDescriptor_t       xDesc,
                                const void                         *x,
                                const cudnnTensorDescriptor_t       dyDesc,
                                const void                         *dy,
                                const cudnnConvolutionDescriptor_t  convDesc,
                                cudnnConvolutionBwdFilterAlgo_t     algo,
                                void                               *workSpace,
                                size_t                              workSpaceSizeInBytes,
                                const void                         *beta,
                                const cudnnFilterDescriptor_t       dwDesc,
                                void                               *dw)
  */
  def cudnnConvolutionBackwardFilter(handle: Rep[cudnnHandleT], alpha: Rep[Array[_]], xDesc: Rep[cudnnTensorDescriptorT], x: Rep[Array[_]], dyDesc: Rep[cudnnFilterDescriptorT],
                                    dy: Rep[Array[_]], convDesc: Rep[cudnnConvolutionDescriptorT], algo: Rep[cudnnConvolutionBwdFilterAlgoT], workspace: Rep[Array[_]], 
                                    workSpaceSizeInBytes: Rep[Int], beta: Rep[Array[_]], dwDesc: Rep[cudnnFilterDescriptorT], dw: Rep[Array[_]]) =
    libFunction[cudnnStatusT]("cudnnConvolutionBackwardFilter", Unwrap(handle), Unwrap(alpha), Unwrap(xDesc), Unwrap(x), Unwrap(dyDesc), Unwrap(dy), Unwrap(convDesc), Unwrap(algo),
      Unwrap(workspace), Unwrap(workSpaceSizeInBytes), Unwrap(beta), Unwrap(dwDesc), Unwrap(dw))(Seq(0,1,2,3,4,5,6,7,8,9,10), Seq(11), Set(1,10))                            

  /*
  cudnnStatus_t cudnnPoolingForward(
                                cudnnHandle_t                    handle,
                                const cudnnPoolingDescriptor_t   poolingDesc,
                                const void                      *alpha,
                                const cudnnTensorDescriptor_t    xDesc,
                                const void                      *x,       // data pointer to input
                                const void                      *beta,
                                const cudnnTensorDescriptor_t    yDesc,
                                void                            *y)       // data pointer to output
  */
  def cudnnPoolingForward(handle: Rep[cudnnHandleT], poolingDesc: Rep[cudnnPoolingDescriptorT], alpha: Rep[Array[_]], xDesc: Rep[cudnnTensorDescriptorT],
                          x: Rep[Array[_]], beta: Rep[Array[_]], yDesc: Rep[cudnnTensorDescriptorT], y: Rep[Array[_]]) =
    libFunction[cudnnStatusT]("cudnnPoolingForward", Unwrap(handle), Unwrap(poolingDesc), Unwrap(alpha), Unwrap(xDesc), Unwrap(x), Unwrap(beta), Unwrap(yDesc),
      Unwrap(y))(Seq(0,1,2,3,4,5,6), Seq(7), Set(2,5))

  /*
  cudnnStatus_t cudnnPoolingBackward(
                                cudnnHandle_t                       handle,
                                const cudnnPoolingDescriptor_t      poolingDesc,
                                const void                         *alpha,
                                const cudnnTensorDescriptor_t       yDesc,
                                const void                         *y,
                                const cudnnTensorDescriptor_t       dyDesc,
                                const void                         *dy,
                                const cudnnTensorDescriptor_t       xDesc,
                                const void                         *xData,
                                const void                         *beta,
                                const cudnnTensorDescriptor_t       dxDesc,
                                void                               *dx)
  */
  def cudnnPoolingBackward(handle: Rep[cudnnHandleT], poolingDesc: Rep[cudnnPoolingDescriptorT], alpha: Rep[Array[_]], yDesc: Rep[cudnnTensorDescriptorT],
                          y: Rep[Array[_]], dyDesc: Rep[cudnnTensorDescriptorT], dy: Rep[Array[_]], xDesc: Rep[cudnnTensorDescriptorT], xData: Rep[Array[_]],
                          beta: Rep[Array[_]], dxDesc: Rep[cudnnTensorDescriptorT], dx: Rep[Array[_]]) =
    libFunction[cudnnStatusT]("cudnnPoolingBackward", Unwrap(handle), Unwrap(poolingDesc),  Unwrap(alpha),  Unwrap(yDesc),  Unwrap(y),  Unwrap(dyDesc),  Unwrap(dy),
       Unwrap(xDesc),  Unwrap(xData), Unwrap(beta),  Unwrap(dxDesc),  Unwrap(dx))(Seq(0,1,2,3,4,5,6,7,8,9,10), Seq(11), Set(2,9))

  /*
  cudnnStatus_t cudnnDropoutForward(
                                cudnnHandle_t                       handle,
                                const cudnnDropoutDescriptor_t      dropoutDesc,
                                const cudnnTensorDescriptor_t       xdesc,
                                const void                         *x,
                                const cudnnTensorDescriptor_t       ydesc,
                                void                               *y,                  // pointer to data of the tensor described by the yDesc descriptor
                                void                               *reserveSpace,       // pointer to user-allocated GPU memory used by this function.
                                size_t                              reserveSpaceSizeInBytes)
  */
  def cudnnDropoutForward(handle: Rep[cudnnHandleT], dropoutDesc: Rep[cudnnDropoutDescriptorT], xdesc: Rep[cudnnTensorDescriptorT], x: Rep[Array[_]], 
                          yDesc: Rep[cudnnTensorDescriptorT], y: Rep[Array[_]], reserveSpace: Rep[Array[_]], reserveSpaceSizeInBytes: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnDropoutForward", Unwrap(handle), Unwrap(dropoutDesc), Unwrap(xdesc), Unwrap(x), Unwrap(yDesc), Unwrap(y), Unwrap(reserveSpace),
      Unwrap(reserveSpaceSizeInBytes))(Seq(0,1,2,3,4,7), Seq(5,6), Set())
  
  /*
  cudnnStatus_t cudnnDropoutBackward(
                                cudnnHandle_t                   handle,
                                const cudnnDropoutDescriptor_t  dropoutDesc,
                                const cudnnTensorDescriptor_t   dydesc,
                                const void                     *dy,
                                const cudnnTensorDescriptor_t   dxdesc,
                                void                           *dx,
                                void                           *reserveSpace,
                                size_t                          reserveSpaceSizeInBytes)
  */
  def cudnnDropoutBackward(handle: Rep[cudnnHandleT], dropoutDesc: Rep[cudnnDropoutDescriptorT], dyDesc: Rep[cudnnTensorDescriptorT], dy: Rep[Array[_]], 
                          dxDesc: Rep[cudnnTensorDescriptorT], dx: Rep[Array[_]], reserveSpace: Rep[Array[_]], reserveSpaceSizeInBytes: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnDropoutBackward", Unwrap(handle), Unwrap(dropoutDesc), Unwrap(dyDesc), Unwrap(dy), Unwrap(dxDesc), Unwrap(dx), Unwrap(reserveSpace),
      Unwrap(reserveSpaceSizeInBytes))(Seq(0,1,2,3,4,6,7), Seq(5), Set())

  /*
  cudnnStatus_t cudnnSoftmaxForward(
                                cudnnHandle_t                    handle,
                                cudnnSoftmaxAlgorithm_t          algorithm,
                                cudnnSoftmaxMode_t               mode,
                                const void                      *alpha,
                                const cudnnTensorDescriptor_t    xDesc,
                                const void                      *x,
                                const void                      *beta,
                                const cudnnTensorDescriptor_t    yDesc,
                                void                            *y)
  */
  def cudnnSoftmaxForward(handle: Rep[cudnnHandleT], algorithm: Rep[cudnnSoftmaxAlgorithmT], mode: Rep[cudnnSoftmaxModeT], alpha: Rep[Array[_]], 
                          xDesc: Rep[cudnnTensorDescriptorT],  x: Rep[_], beta: Rep[Array[_]], yDesc: Rep[cudnnTensorDescriptorT], y: Rep[_]) =
    libFunction[cudnnStatusT]("cudnnSoftmaxForward", Unwrap(handle), Unwrap(algorithm), Unwrap(mode), Unwrap(alpha), Unwrap(xDesc), 
      Unwrap(x), Unwrap(beta), Unwrap(yDesc), Unwrap(y))(Seq(0,1,2,3,4,5,6,7), Seq(8), Set(3,5,6,8))
  
  /*
  cudnnStatus_t cudnnSoftmaxBackward(
                                cudnnHandle_t                    handle,
                                cudnnSoftmaxAlgorithm_t          algorithm,
                                cudnnSoftmaxMode_t               mode,
                                const void                      *alpha,
                                const cudnnTensorDescriptor_t    yDesc,
                                const void                      *yData,
                                const cudnnTensorDescriptor_t    dyDesc,
                                const void                      *dy,
                                const void                      *beta,
                                const cudnnTensorDescriptor_t    dxDesc,
                                void                            *dx)
  */
  def cudnnSoftmaxBackward(handle: Rep[cudnnHandleT], algorithm: Rep[cudnnSoftmaxAlgorithmT], mode: Rep[cudnnSoftmaxModeT], alpha: Rep[Array[_]],
                          yDesc: Rep[cudnnTensorDescriptorT], yData: Var[_], dyDesc: Rep[cudnnTensorDescriptorT], dy: Var[_], beta: Rep[Array[_]],
                          dxDesc: Rep[cudnnTensorDescriptorT], dx: Var[_]) =
    libFunction[cudnnStatusT]("cudnnSoftmaxBackward", Unwrap(handle), Unwrap(algorithm), Unwrap(mode), Unwrap(alpha), Unwrap(yDesc), 
      Unwrap(yData), Unwrap(dyDesc), UnwrapV(dy), Unwrap(beta), Unwrap(dxDesc), UnwrapV(dx))(Seq(0,1,2,3,4,5,6,7,8,9), Seq(10), Set(3,4,5,7,8,10))

}

trait CCodeGenCUDNN extends ExtendedCCodeGen {

  registerHeader("\"cudnn_header.h\"")

  override def remap(m: Manifest[_]) = m.runtimeClass.getName match {
    case s: String if s.endsWith("$cudnnConvolutionFwdAlgoPerfT") => "cudnnConvolutionFwdAlgoPerf_t"
    case s: String if s.endsWith("$cudnnConvolutionFwdAlgoT") => "cudnnConvolutionFwdAlgo_t"
    case _ => super.remap(m)
  }
}