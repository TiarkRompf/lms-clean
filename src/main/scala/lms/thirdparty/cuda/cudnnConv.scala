package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

trait CUDNNConvTypeLess extends Dsl with CLibs with CUDNNBaseTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class CUDNN_CONV_DESCRIPTOR(override val x: Backend.Exp) extends TOP(x)

  class CUDNN_CONV_FWD_ALG_PERF(override val x: Backend.Exp) extends TOP(x)
  class CUDNN_CONV_FWD_ALGO(override val x: Backend.Exp) extends TOP(x)
  class CUDNN_CONV_BWD_DATA_ALG_PERF(override val x: Backend.Exp) extends TOP(x)
  class CUDNN_CONV_BWD_DATA_ALGO(override val x: Backend.Exp) extends TOP(x)
  class CUDNN_CONV_BWD_FILTER_ALG_PERF(override val x: Backend.Exp) extends TOP(x)
  class CUDNN_CONV_BWD_FILTER_ALGO(override val x: Backend.Exp) extends TOP(x)


  def CUDNN_CONVOLUTION(implicit __pos: SourceContext) = CMACRO("CUDNN_CONVOLUTION", manifest[Int])
  def CUDNN_CROSS_CORRELATION(implicit __pos: SourceContext) = CMACRO("CUDNN_CROSS_CORRELATION", manifest[Int])

  def CUDNN_CREATE_CONV_DESCRIPTOR(convDesc: CUDNN_CONV_DESCRIPTOR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnCreateConvolutionDescriptor", convDesc.x)(Seq(), Seq(0), Set[Int](0))
  
  def CUDNN_DESTROY_CONV_DESCRIPTOR(convDesc: TOP)(implicit __pos: SourceContext) = 
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDestroyConvolutionDescriptor", convDesc.x)(Seq(), Seq(0), Set[Int]())

  def CUDNN_SET_CONV_2D_DESCRIPTOR(convDesc: CUDNN_CONV_DESCRIPTOR, pad_h: INT, pad_w: INT, u: INT, v: INT, dilation_h: INT, 
                                  dilation_w: INT, mode: TOP, computeType: TOP)(implicit __pos: SourceContext) =
  LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSetConvolution2dDescriptor", convDesc.x, pad_h.x, pad_w.x, u.x, v.x,
    dilation_h.x, dilation_w.x, mode.x, computeType.x)(Seq(1,2,3,4,5,6,7,8), Seq(0), Set[Int]())
  

  def CUDNN_GET_CONV_2D_FWD_OUTPUT_DIM(convDesc: CUDNN_CONV_DESCRIPTOR, inputTensorDesc: TOP, 
                                      filterDesc: TOP, n: INT, c: INT, h: INT, w: INT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnGetConvolution2dForwardOutputDim", convDesc.x, inputTensorDesc.x, 
      filterDesc.x, n.x, c.x, h.x, w.x)(Seq(0,1,2), Seq(3,4,5,6), Set[Int](3,4,5,6))

  def CUDNN_FIND_CONV_FWD_ALG(handle: TOP, xDesc: TOP, wDesc: TOP,
                               convDesc: CUDNN_CONV_DESCRIPTOR, yDesc: TOP, requestedAlgoCount: INT,
                               returnedAlgoCount: VAR, perfResults: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnFindConvolutionForwardAlgorithm", handle.x, xDesc.x, wDesc.x, convDesc.x,
      yDesc.x, requestedAlgoCount.x, returnedAlgoCount.x, perfResults.x)(Seq(0,1,2,3,4,5,7), Seq(6,7), Set[Int](6,7))

  def CUDNN_GET_CONV_FWD_WORKSPACE_SZ(handle: TOP, xDesc: TOP, wDesc: TOP,
                                      convDesc: CUDNN_CONV_DESCRIPTOR, yDesc: TOP, algo: TOP, 
                                      sizeInBytes: VAR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnGetConvolutionForwardWorkspaceSize", handle.x, xDesc.x, wDesc.x, convDesc.x,
      yDesc.x, algo.x, sizeInBytes.x)(Seq(0,1,2,3,4,5), Seq(6), Set[Int](6))

  def CUDNN_CONV_FWD(handle: TOP, alpha: VAR, xDesc: TOP, x: TOP,
                              wDesc: TOP, w: TOP, convDesc: CUDNN_CONV_DESCRIPTOR,
                              algo: TOP, workspace: TOP, workSpaceSizeInBytes: VAR,
                              beta: VAR, yDesc: TOP, y: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnConvolutionForward", handle.x, alpha.x, xDesc.x, x.x, wDesc.x, w.x, convDesc.x,
      algo.x, workspace.x, workSpaceSizeInBytes.x, beta.x, yDesc.x, y.x)(Seq(0,1,2,3,4,5,6,7,8,9,10,11), Seq(1,5,12),
      Set[Int](1,10))
  
  
  def CUDNN_FIND_CONV_BWD_DATA_ALG(handle: TOP, wDesc: TOP, dyDesc: TOP, convDesc: CUDNN_CONV_DESCRIPTOR,
                                  dxDesc: TOP, requestedAlgoCount: INT, returnedAlgoCount: VAR, perfResults: TOP)
                                  (implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnFindConvolutionBackwardDataAlgorithm", handle.x, wDesc.x, dyDesc.x, convDesc.x,
      dxDesc.x, requestedAlgoCount.x, returnedAlgoCount.x, perfResults.x)(Seq(0,1,2,3,4,5,7), Seq(6,7), Set[Int](6,7))

  def CUDNN_GET_CONV_BWD_DATA_WORKSPACE_SZ(handle: TOP, wDesc: TOP, dyDesc: TOP,
                                      convDesc: CUDNN_CONV_DESCRIPTOR, dxDesc: TOP, algo: TOP, 
                                      sizeInBytes: VAR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnGetConvolutionBackwardDataWorkspaceSize", handle.x, wDesc.x, dyDesc.x, convDesc.x,
      dxDesc.x, algo.x, sizeInBytes.x)(Seq(0,1,2,3,4,5), Seq(6), Set[Int](6))

  def CUDNN_CONV_BWD_DATA(handle: TOP, alpha: VAR, wDesc: TOP, w: TOP,
                                  dyDesc: TOP, dy: TOP, convDesc: CUDNN_CONV_DESCRIPTOR,
                                  algo: TOP, workspace: TOP, workSpaceSizeInBytes: VAR, 
                                  beta: VAR, dxDesc: TOP, dx: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnConvolutionBackwardData", handle.x, alpha.x, wDesc.x, w.x, dyDesc.x, dy.x, 
      convDesc.x, algo.x, workspace.x, workSpaceSizeInBytes.x, beta.x, dxDesc.x, dx.x)(Seq(0,1,2,3,4,5,6,7,8,9,10,11), Seq(12), 
      Set[Int](1,10))


  def CUDNN_FIND_CONV_BWD_FILTER_ALG(handle: TOP, xDesc: TOP, dyDesc: TOP, convDesc: CUDNN_CONV_DESCRIPTOR,
                                  dwDesc: TOP, requestedAlgoCount: INT, returnedAlgoCount: VAR, perfResults: TOP)
                                  (implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnFindConvolutionBackwardFilterAlgorithm", handle.x, xDesc.x, dyDesc.x, convDesc.x,
      dwDesc.x, requestedAlgoCount.x, returnedAlgoCount.x, perfResults.x)(Seq(0,1,2,3,4,5,7), Seq(6,7), Set[Int](6,7))

  def CUDNN_GET_CONV_BWD_FILTER_WORKSPACE_SZ(handle: TOP, xDesc: TOP, dyDesc: TOP,
                                      convDesc: CUDNN_CONV_DESCRIPTOR, dwDesc: TOP, algo: TOP, 
                                      sizeInBytes: VAR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnGetConvolutionBackwardFilterWorkspaceSize", handle.x, xDesc.x, dyDesc.x, convDesc.x,
      dwDesc.x, algo.x, sizeInBytes.x)(Seq(0,1,2,3,4,5), Seq(6), Set[Int](6))


  def CUDNN_CONV_BWD_FILTER(handle: TOP, alpha: VAR, xDesc: TOP, x: TOP,
                                  dyDesc: TOP, dy: TOP, convDesc: CUDNN_CONV_DESCRIPTOR,
                                  algo: TOP, workspace: TOP, workSpaceSizeInBytes: VAR, 
                                  beta: VAR, dwDesc: TOP, dw: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnConvolutionBackwardFilter", handle.x, alpha.x, xDesc.x, x.x, dyDesc.x, dy.x, 
      convDesc.x, algo.x, workspace.x, workSpaceSizeInBytes.x, beta.x, dwDesc.x, dw.x)(Seq(0,1,2,3,4,5,6,7,8,9,10,11), Seq(12), 
      Set[Int](1,10))
}

trait CUDNNConvOps extends CLibs with CudaOps with CUDNNBaseOps {

  class cudnnConvolutionDescriptorT
  def cudnnConvolutionDescriptor: Rep[cudnnConvolutionDescriptorT] = newStruct[cudnnConvolutionDescriptorT]("cudnnConvolutionDescriptor_t")

  class cudnnConvolutionModeT
  def cudnnConvolution = cmacro[cudnnConvolutionModeT]("CUDNN_CONVOLUTION")
  def cudnnCrossCorrelation = cmacro[cudnnConvolutionModeT]("CUDNN_CROSS_CORRELATION")

  class cudnnConvolutionFwdAlgoT
  def cudnnConvolutionFwdAlgo = newStruct[cudnnConvolutionFwdAlgoT]("cudnnConvolutionFwdAlgo_t")

  class cudnnConvolutionFwdAlgoPerfT
  def cudnnConvolutionFwdAlgoPerf = newStruct[cudnnConvolutionFwdAlgoPerfT]("cudnnConvolutionFwdAlgoPerf_t")

  class cudnnConvolutionBwdFilterAlgoPerfT

  class cudnnConvolutionBwdFilterAlgoT
  def cudnnConvolutionBwdFilterAlgo0 = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_0")
  def cudnnConvolutionBwdFilterAlgo1 = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_1")
  def cudnnConvolutionBwdFilterAlgoFFT = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_FFT")
  def cudnnConvolutionBwdFilterAlgo3 = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_3")
  def cudnnConvolutionBwdFilterWinogradNonfused = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_WINOGRAD_NONFUSED")
  def cudnnConvolutionBwdFilterAlgoFFTTiling = cmacro[cudnnConvolutionBwdFilterAlgoT]("CUDNN_CONVOLUTION_BWD_FILTER_ALGO_FFT_TILING")

  class cudnnConvolutionBwdDataAlgoPerfT

  class cudnnConvolutionBwdDataAlgoT


  // cudnnStatus_t cudnnCreateConvolutionDescriptor(cudnnConvolutionDescriptor_t *convDesc)
  def cudnnCreateConvolutionDescriptor(convDesc: Rep[cudnnConvolutionDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreateConvolutionDescriptor", Unwrap(convDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyConvolutionDescriptor(cudnnConvolutionDescriptor_t convDesc)
  def cudnnDestroyConvolutionDescriptor(convDesc: Rep[cudnnConvolutionDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyConvolutionDescriptor", Unwrap(convDesc))(Seq(), Seq(0), Set())
  
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

}

trait CCodeGenCUDNNConv extends ExtendedCCodeGen {

  override def remap(m: Manifest[_]) = m.runtimeClass.getName match {
    case s: String if s.endsWith("$cudnnConvolutionFwdAlgoPerfT") ||
      s.endsWith("$CUDNN_CONV_FWD_ALG_PERF")=> "cudnnConvolutionFwdAlgoPerf_t"
    
    case s: String if s.endsWith("$cudnnConvolutionFwdAlgoT") ||
      s.endsWith("$CUDNN_CONV_FWD_ALGO") => "cudnnConvolutionFwdAlgo_t"
    
    case s: String if s.endsWith("$cudnnConvolutionBwdDataAlgoPerfT") ||
      s.endsWith("$CUDNN_CONV_BWD_DATA_ALG_PERF") => "cudnnConvolutionBwdDataAlgoPerf_t"
    
    case s: String if s.endsWith("$cudnnConvolutionBwdDataAlgoT") ||
      s.endsWith("$CUDNN_CONV_BWD_DATA_ALGO") => "cudnnConvolutionBwdDataAlgo_t"
    
    case s: String if s.endsWith("$cudnnConvolutionBwdFilterAlgoPerfT") ||
      s.endsWith("$CUDNN_CONV_BWD_FILTER_ALG_PERF") => "cudnnConvolutionBwdFilterAlgoPerf_t"
    
    case s: String if s.endsWith("$cudnnConvolutionBwdFilterAlgoT") ||
      s.endsWith("$CUDNN_CONV_BWD_FILTER_ALGO") => "cudnnConvolutionBwdFilterAlgo_t"
    
    case s => super.remap(m)
  }
}
