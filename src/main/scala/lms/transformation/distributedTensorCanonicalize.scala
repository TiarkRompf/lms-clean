package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributedTensorCanonicalize extends Transformer {
  override val name = "DistributedTensorCanonicalize"

  import FixedSizeDistributedTensorTypeLess._

  override def transform(n: Node): Exp = n match {
    case Node(s, "op_split", _, _) =>
      val ts = super.transform(n)
      val dummy = new SPLIT_OP(ts)
      ts
    case _ => super.transform(n)
  }

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g

    // handle the metadata in OPERATION.resultMap
    OPERATION.oldResultMap = OPERATION.resultMap
    OPERATION.resultMap = new mutable.HashMap[lms.core.Backend.Exp, List[lms.core.Backend.Exp]]()

    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
