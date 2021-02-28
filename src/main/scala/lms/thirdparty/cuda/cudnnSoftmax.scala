package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

trait CUDNNSoftmaxTypeLess extends Dsl with CLibs with CUDNNBaseTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  def CUDNN_SOFTMAX_FAST(implicit __pos: SourceContext) = CMACRO("CUDNN_SOFTMAX_FAST", manifest[Int])
  def CUDNN_SOFTMAX_MODE_INSTANCE(implicit __pos: SourceContext) = CMACRO("CUDNN_SOFTMAX_MODE_INSTANCE", manifest[Int])

  def CUDNN_SOFTMAX_FWD(handle: TOP, algorithm: TOP, mode: TOP, alpha: VAR, xDesc: TOP, 
                        x: TOP, beta: VAR, yDesc: TOP, y: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSoftmaxForward", handle.x, algorithm.x, mode.x, alpha.x, xDesc.x,
      x.x, beta.x, yDesc.x, y.x)(Seq(0,1,2,3,4,5,6,7), Seq(8), Set[Int](3,5,6,8))
  
  def CUDNN_SOFTMAX_BWD(handle: TOP, algorithm: TOP, mode: TOP, alpha: VAR, yDesc: TOP,
                        yData: TOP, dyDesc: TOP, dy: TOP, beta: VAR, dxDesc: TOP, dx: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSoftmaxBackward", handle.x, algorithm.x, mode.x, alpha.x, yDesc.x,
      yData.x, dyDesc.x, dy.x, beta.x, dxDesc.x, dx.x)(Seq(0,1,2,3,4,5,6,7,8,9), Seq(10), Set(3,5,7,8,10))
                    

  
}

trait CUDNNSoftmaxOps extends CLibs with CudaOps with CUDNNBaseOps {

  class cudnnSoftmaxAlgorithmT
  def cudnnSoftmaxFast = cmacro[cudnnSoftmaxAlgorithmT]("CUDNN_SOFTMAX_FAST")
  def cudnnSoftmaxAccurate = cmacro[cudnnSoftmaxAlgorithmT]("CUDNN_SOFTMAX_ACCURATE")
  def cudnnSoftmaxLog = cmacro[cudnnSoftmaxAlgorithmT]("CUDNN_SOFTMAX_LOG")

  class cudnnSoftmaxModeT
  def cudnnSoftmaxModeInstance = cmacro[cudnnSoftmaxModeT]("CUDNN_SOFTMAX_MODE_INSTANCE")
  def cudnnsoftmaxModeChannel = cmacro[cudnnSoftmaxModeT]("CUDNN_SOFTMAX_MODE_CHANNEL")

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
