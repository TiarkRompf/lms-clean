package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

trait CUDNNActivationTypeLess extends Dsl with CLibs with CUDNNBaseTypeLess {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class CUDNN_ACTIVATION_DESCRIPTOR(override val x: Backend.Exp) extends TOP(x)

  def CUDNN_ACTIVATION_SIGMOID(implicit __pos: SourceContext) = CMACRO("CUDNN_ACTIVATION_SIGMOID", manifest[Int])
  def CUDNN_ACTIVATION_RELU(implicit __pos: SourceContext) = CMACRO("CUDNN_ACTIVATION_RELU", manifest[Int])
  def CUDNN_ACTIVATION_TANH(implicit __pos: SourceContext) = CMACRO("CUDNN_ACTIVATION_TANH", manifest[Int])
  def CUDNN_ACTIVATION_CLIPPED_RELU(implicit __pos: SourceContext) = CMACRO("CUDNN_ACTIVATION_CLIPPED_RELU", manifest[Int])
  def CUDNN_ACTIVATION_ELU(implicit __pos: SourceContext) = CMACRO("CUDNN_ACTIVATION_ELU", manifest[Int])
  def CUDNN_ACTIVATION_IDENTITY(implicit __pos: SourceContext) = CMACRO("CUDNN_ACTIVATION_IDENTITY", manifest[Int])


  def CUDNN_CREATE_ACTIVATION_DESCRIPTOR(activationDesc: CUDNN_ACTIVATION_DESCRIPTOR)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnCreateActivationDescriptor", activationDesc.x)(Seq(), Seq(0), Set[Int](0))

  def CUDNN_DESTROY_ACTIVATION_DESCRIPTOR(activationDesc: CUDNN_ACTIVATION_DESCRIPTOR) = {
    implicit val pos: SourceContext = Adapter.sourceMap.getOrElse(activationDesc.x, Adapter.oldSourceMap(activationDesc.x))
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnDestroyActivationDescriptor", activationDesc.x)(Seq(), Seq(0), Set[Int](), Adapter.CTRL)
  }

  def CUDNN_SET_ACTIVATION_DESCRIPTOR(activationDesc: CUDNN_ACTIVATION_DESCRIPTOR, mode: TOP, reluNanOpt: TOP,
                                      coef: FLOAT)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnSetActivationDescriptor", activationDesc.x, mode.x, reluNanOpt.x,
      coef.x)(Seq(1,2,3), Seq(0), Set[Int]())

  def CUDNN_ACTIVATION_FWD(handle: TOP, activationDesc: CUDNN_ACTIVATION_DESCRIPTOR, alpha: VAR, xDesc: TOP, x: TOP,
                          beta: VAR, yDesc: TOP, y: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnActivationForward", handle.x, activationDesc.x, alpha.x, xDesc.x, x.x,
      beta.x, yDesc.x, y.x)(Seq(0,1,2,3,4,5), Seq(6), Set[Int](2,5))

  def CUDNN_ACTIVATION_BWD(handle: TOP, activationDesc: CUDNN_ACTIVATION_DESCRIPTOR, alpha: VAR, yDesc: TOP, y: TOP,
                          dyDesc: TOP, dy: TOP, xDesc: TOP, x: TOP, beta: VAR, dxDesc: TOP, dx: TOP)(implicit __pos: SourceContext) =
    LIB_FUNCTION(manifest[CUDNN_RESULT], "cudnnActivationBackward", handle.x, activationDesc.x, alpha.x, yDesc.x, y.x,
      dyDesc.x, dy.x, xDesc.x, x.x, beta.x, dxDesc.x, dx.x)(Seq(0,1,2,3,4,5,6,7,8,9,10), Seq(11), Set[Int](2,9))

}

trait CUDNNActivationOps extends CLibs with CudaOps with CUDNNBaseOps {

  class cudnnActivationDescriptorT
  def cudnnActivationDescriptor: Rep[cudnnActivationDescriptorT] = newStruct[cudnnActivationDescriptorT]("cudnnActivationDescriptor_t")

  class cudnnActivationModeT
  def cudnnActivationSigmoid = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_SIGMOID")
  def cudnnActivationRelu = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_RELU")
  def cudnnActivationTanh = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_RELU")
  def cudnnActivationClippedRelu = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_CLIPPED_RELU")
  def cudnnActivationElu = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_ELU")
  def cudnnActivationIdentity = cmacro[cudnnActivationModeT]("CUDNN_ACTIVATION_IDENTITY")

  // cudnnStatus_t cudnnCreateActivationDescriptor(cudnnActivationDescriptor_t *activationDesc)
  def cudnnCreateActivationDescriptor(activationDesc: Rep[cudnnActivationDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnCreateActivationDescriptor", Unwrap(activationDesc))(Seq(), Seq(0), Set(0))

  // cudnnStatus_t cudnnDestroyActivationDescriptor(cudnnActivationDescriptor_t activationDesc)
  def cudnnDestroyActivationDescriptor(activationDesc: Rep[cudnnActivationDescriptorT]) =
    libFunction[cudnnStatusT]("cudnnDestroyActivationDescriptor", Unwrap(activationDesc))(Seq(), Seq(0), Set())

}
