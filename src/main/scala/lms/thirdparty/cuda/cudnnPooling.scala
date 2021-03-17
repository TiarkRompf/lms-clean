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

  def CUDNN_POOLING_MAX(implicit __pos: SourceContext) = CMACRO("CUDNN_POOLING_MAX", manifest[Int])
  def CUDNN_POOLING_AVERAGE_COUNT_INCLUDE_PADDING(implicit __pos: SourceContext) = CMACRO("CUDNN_POOLING_AVERAGE_COUNT_INCLUDE_PADDING", manifest[Int])
  def CUDNN_POOLING_AVERAGE_COUNT_EXCLUDE_PADDING(implicit __pos: SourceContext) = CMACRO("CUDNN_POOLING_AVERAGE_COUNT_EXCLUDE_PADDING", manifest[Int])
  def CUDNN_POOLING_MAX_DETERMINISTIC(implicit __pos: SourceContext) = CMACRO("CUDNN_POOLING_MAX_DETERMINISTIC", manifest[Int])

  def CUDNN_CREATE_POOLING_DESCRIPTOR(poolingDesc: CUDNN_POOLING_DESCRIPTOR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnCreatePoolingDescriptor", poolingDesc.x)(Seq(), Seq(0), Set[Int](0))

  def CUDNN_DESTROY_POOLING_DESCRIPTOR(poolingDesc: TOP) = {
    implicit val pos: SourceContext = Adapter.sourceMap.getOrElse(poolingDesc.x, Adapter.oldSourceMap(poolingDesc.x))
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDestroyPoolingDescriptor", poolingDesc.x)(Seq(), Seq(0), Set[Int](), Adapter.CTRL)
  }

  def CUDNN_SET_POOLING_2D_DESCRIPTOR(poolingDesc: CUDNN_POOLING_DESCRIPTOR, mode: TOP, maxpoolingNanOpt: TOP, windowHeight: INT, windowWidth: INT,
                                      verticalPadding: INT, horizontalPadding: INT, verticalStride: INT, horizontalStride: INT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSetPooling2dDescriptor", poolingDesc.x, mode.x, maxpoolingNanOpt.x, windowHeight.x, windowWidth.x,
      verticalPadding.x, horizontalPadding.x, verticalStride.x, horizontalStride.x)(Seq(1,2,3,4,5,6,7,8), Seq(0), Set[Int]())

  def CUDNN_GET_POOLING_2D_FWD_OUTPUT_DIM(poolingDesc: CUDNN_POOLING_DESCRIPTOR, inputDesc: TOP, n: INT, c: INT, h: INT, w: INT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnGetPooling2dForwardOutputDim", poolingDesc.x, inputDesc.x, n.x, c.x, h.x, w.x)(Seq(0,1), Seq(2,3,4,5),
      Set[Int](2,3,4,5))

  def CUDNN_POOLING_FWD(handle: TOP, poolingDesc: CUDNN_POOLING_DESCRIPTOR, alpha: VAR, xDesc: TOP,
                          x: TOP, beta: VAR, yDesc: TOP, y: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnPoolingForward", handle.x, poolingDesc.x, alpha.x, xDesc.x, x.x, beta.x, yDesc.x,
      y.x)(Seq(0,1,2,3,4,5,6), Seq(2,5,7), Set[Int](2,5))

    def CUDNN_POOLING_BWD(handle: TOP, poolingDesc: CUDNN_POOLING_DESCRIPTOR, alpha: VAR, yDesc: TOP,
                          y: TOP, dyDesc: TOP, dy: TOP, xDesc: TOP, xData: TOP,
                          beta: VAR, dxDesc: TOP, dx: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnPoolingBackward", handle.x, poolingDesc.x, alpha.x, yDesc.x, y.x, dyDesc.x, dy.x,
      xDesc.x, xData.x, beta.x, dxDesc.x, dx.x)(Seq(0,1,2,3,4,5,6,7,8,9,10), Seq(11), Set[Int](2,9))

}

trait CUDNNPoolingOps extends CLibs with CudaOps with CUDNNBaseOps {

  class cudnnPoolingDescriptorT
  def cudnnPoolingDescriptor: Rep[cudnnPoolingDescriptorT] = newStruct[cudnnPoolingDescriptorT]("cudnnPoolingDescriptor_t")

  class cudnnPoolingModeT
  def cudnnPoolingMax = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_MAX")
  def cudnnPoolingAvgCntInPadding = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_AVERAGE_COUNT_INCLUDE_PADDING")
  def cudnnPoolingAvgCntExPadding = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_AVERAGE_COUNT_EXCLUDE_PADDING")
  def cudnnPoolingMaxDeterministic = cmacro[cudnnPoolingModeT]("CUDNN_POOLING_MAX_DETERMINISTIC")

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
