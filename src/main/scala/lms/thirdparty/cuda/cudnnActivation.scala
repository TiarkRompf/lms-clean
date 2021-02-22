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
