package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorConcat extends Transformer {

  override val name = "FusedTensorConcat"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._

  val splits = new mutable.HashMap[(Backend.Sym, Int), TENSOR]
  val results = new mutable.HashMap[Backend.Sym, TENSOR]

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_concat", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x), useOldMetadata = true)
      val b = new TENSOR(transform(y), useOldMetadata = true)

      require(a.size.last == b.size.head && a.inputs == b.inputs, "cannot concat")

      System.out.println("a: " + a.body)
      System.out.println("b: " + b.body)

      val Backend.Block(a_arg::Nil, a_r, _, _) = a.body
      val Backend.Block(b_arg::Nil, b_r, _, _) = b.body

      val sz = a.size.sum + b.size.sum
      val res = TENSOR(Seq(0, sz), a.inputs){ i =>
        (IF(INT(i) < INT(a.size.sum))(a.apply(i))(b.apply(i))).x
      }
      res.x
    
    case _ => super.transform(n)
  }

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g
    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
