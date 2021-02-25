package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

object CUDNNTypeLess extends CUDNNBaseTypeLess
  with CUDNNConvTypeLess
  with CUDNNDropoutTypeLess
  with CUDNNPoolingTypeLess 
  with CUDNNActivationTypeLess
  with CUDNNSoftmaxTypeLess

trait CUDNNOps extends CUDNNBaseOps
  with CUDNNConvOps
  with CUDNNDropoutOps
  with CUDNNPoolingOps
  with CUDNNActivationOps
  with CUDNNSoftmaxOps

trait CCodeGenCUDNN extends CCodeGenCUDNNConv {
  registerHeader("\"cudnn_header.h\"")
}
