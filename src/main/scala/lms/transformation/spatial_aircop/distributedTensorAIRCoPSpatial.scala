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
    with DistributeTensorAIRCoPSpatialGemm
    with DistributeTensorAIRCoPSpatialMutation
    with DistributeTensorAIRCoPSplitConcat
    with DistributeTensorAIRCoPSpatialUnary
    with DistributeTensorAIRCoPSpatialConv
    with DistributeTensorAIRCoPSpatialMiscs
