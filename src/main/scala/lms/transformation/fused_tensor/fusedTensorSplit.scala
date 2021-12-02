package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorSplit extends Transformer {

  override val name = "FusedTensorSplit"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._

  val splits = new mutable.HashMap[(Backend.Sym, Int), TENSOR]
  val results = new mutable.HashMap[Backend.Sym, TENSOR]

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_split", (x:Backend.Sym)::(Backend.Const(sz:Seq[Int]))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(x, useOldMetadata = true)
      val t1 = TENSOR(t.size, t.inputs){ i => t.apply(INT(i).x).x }
      val t2 = TENSOR(t.size, t.inputs){ i => t.apply(INT(i).x).x }
      splits((s, 0)) = t1
      splits((s, 1)) = t2
      // System.out.println("t1: " + t1)
      TENSORS(Seq(t1.x, t2.x)).x
    
    case Node(s, "tensor_result",(x:Backend.Sym)::(Backend.Const(i:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = splits((x, i))
      System.out.println("t:" + t)
      System.out.println("s:" + s)
      t.x

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