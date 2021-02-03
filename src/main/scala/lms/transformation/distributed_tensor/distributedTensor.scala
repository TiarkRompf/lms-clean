package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

/**
 * In this frondend design, we are building Tensor IR with fixed shape and device.
 *
 * In the first step, we are simply supporting GPU and CPU.
 * Both Tensor IR and Tensor computation IR have device attributes.
 * We hope to resolve tensor communication automatically during transformation.
 *    such that: tensors are moved to GPU or CPU based on the request from tensor computation
 *               tensors can be printed only from CPU.
 */
object FixedSizeDistributedTensorTypeLess extends FixedSizeDistributedTensorBaseTypeLess
  with FixedSizeDistributedTensorBinaryTypeLess
  with FixedSizeDistributedTensorGemmTypeLess
  with FixedSizeDistributedTensorMutationTypeLess
  with FixedSizeDistributedTensorUnaryTypeLess
  with FixedSizeDistributedTensorConvTypeLess

trait FixedSizeDistributedTensorOps extends FixedSizeDistributedTensorOpsBase
  with FixedSizeDistributedTensorOpsBinary
  with FixedSizeDistributedTensorOpsGemm
  with FixedSizeDistributedTensorOpsMutation
  with FixedSizeDistributedTensorOpsUnary

