package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.ArrayCPUOps

import Backend._

abstract class TensorResolvingDevice extends Transformer with FixedSizeTensorDeviceFrontEnd {

  // LATER: let's first have a IR that manually resolve device

}
