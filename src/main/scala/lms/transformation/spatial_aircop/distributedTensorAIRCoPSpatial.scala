package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensorAIRCoPSpatial extends DistributeTensorAIRCoPSpatialBase
    with DistributeTensorAIRCoPSpatialBinary
    // with DistributeTensor2MPI_NCCLGemm
    // with DistributeTensor2MPI_NCCLMutation
    // with DistributeTensor2MPI_NCCLSplitConcat
    with DistributeTensorAIRCoPSpatialUnary
    with DistributeTensorAIRCoPSpatialConv
