package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

trait CUDNNPoolingTypeLess extends Dsl with CLibs with CUDNNBaseTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class CUDNN_POOLING_DESCRIPTOR(override val x: Backend.Exp) extends TOP(x)
}

trait CUDNNPoolingOps extends CLibs with CudaOps with CUDNNBaseOps {

  class cudnnPoolingDescriptorT
  def cudnnPoolingDescriptor: Rep[cudnnPoolingDescriptorT] = newStruct[cudnnPoolingDescriptorT]("cudnnPoolingDescriptor_t")

  class cudnnPoolingModeT
  def cudnnPoolingMax = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_MAX")
  def cudnnPoolingAvgCntInPadding = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_AVERAGE_COUNT_INCLUDE_PADDING")
  def cudnnPoolingAvgCntExPadding = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_AVERAGE_COUNT_EXCLUDE_PADDING")
  def cudnnPoolingMaxDeterministic = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_MAX_DETERMINISTIC")

  class cudnnNanPropagationT
  def cudnnPropagateNan = cmacro[cudnnNanPropagationT]("CUDNN_PROPAGATE_NAN")
  def cudnnNotPropagateNan = cmacro[cudnnNanPropagationT]("CUDNN_NOT_PROPAGATE_NAN")

  // cudnnStatus_t cudnnCreatePoolingDescriptor(cudnnPoolingDescriptor_t    *poolingDesc)
  def cudnnCreatePoolingDescriptor(poolingDesc: Rep[cudnnPoolingDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreatePoolingDescriptor", Unwrap(poolingDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyPoolingDescriptor(cudnnConvolutionDescriptor_t poolingDesc)
  def cudnnDestroyPoolingDescriptor(poolingDesc: Rep[cudnnPoolingDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyPoolingDescriptor", Unwrap(poolingDesc))(Seq(), Seq(0), Set())
  
  /*
  cudnnStatus_t cudnnSetPooling2dDescriptor(
                                cudnnPoolingDescriptor_t    poolingDesc,
                                cudnnPoolingMode_t          mode,
                                cudnnNanPropagation_t       maxpoolingNanOpt,
                                int                         windowHeight,
                                int                         windowWidth,
                                int                         verticalPadding,
                                int                         horizontalPadding,
                                int                         verticalStride,
                                int                         horizontalStride)
  */
  def cudnnSetPooling2dDescriptor(poolingDesc: Rep[cudnnPoolingDescriptorT], mode: Rep[cudnnPoolingModeT], maxpoolingNanOpt: Rep[cudnnNanPropagationT], windowHeight: Rep[Int],
                                  windowWidth: Rep[Int], verticalPadding: Rep[Int], horizontalPadding: Rep[Int], verticalStride: Rep[Int], horizontalStride: Rep[Int]) =
    libFunction[cudnnStatusT]("cudnnSetPooling2dDescriptor", Unwrap(poolingDesc), Unwrap(mode), Unwrap(maxpoolingNanOpt), Unwrap(windowHeight), Unwrap(windowWidth),
      Unwrap(verticalPadding), Unwrap(horizontalPadding), Unwrap(verticalStride), Unwrap(horizontalStride))(Seq(1,2,3,4,5,6,7,8), Seq(0), Set())
  

  /*
  cudnnStatus_t cudnnGetPooling2dForwardOutputDim(
                                const cudnnPoolingDescriptor_t      poolingDesc,
                                const cudnnTensorDescriptor_t       inputDesc,
                                int                                *outN,
                                int                                *outC,
                                int                                *outH,
                                int                                *outW)
  */
  def cudnnGetPooling2dForwardOutputDim(poolingDesc: Rep[cudnnPoolingDescriptorT], inputDesc: Rep[cudnnTensorDescriptorT], n: Var[Int], c: Var[Int], h: Var[Int], w: Var[Int]) =
    libFunction[cudnnStatusT]("cudnnGetPooling2dForwardOutputDim", Unwrap(poolingDesc), Unwrap(inputDesc), UnwrapV(n), UnwrapV(c), UnwrapV(h), UnwrapV(w))(Seq(0,1), Seq(2,3,4,5),
    Set(2,3,4,5))

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
  def cudnnPoolingForward(handle: Rep[cudnnHandleT], poolingDesc: Rep[cudnnPoolingDescriptorT], alpha: Var[Float], xDesc: Rep[cudnnTensorDescriptorT],
                          x: Rep[Array[_]], beta: Var[Float], yDesc: Rep[cudnnTensorDescriptorT], y: Rep[Array[_]]) =
    libFunction[cudnnStatusT]("cudnnPoolingForward", Unwrap(handle), Unwrap(poolingDesc), UnwrapV(alpha), Unwrap(xDesc), Unwrap(x), UnwrapV(beta), Unwrap(yDesc),
      Unwrap(y))(Seq(0,1,2,3,4,5,6), Seq(2,5,7), Set(2,5))

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
}