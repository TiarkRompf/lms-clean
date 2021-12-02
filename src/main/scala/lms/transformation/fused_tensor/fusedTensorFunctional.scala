package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorFunctional extends Transformer {

  override val name = "FusedTensorFunctional"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_zeros", (Backend.Const(sz:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(sz, Seq())(i => INT(0).x)
      t.x
    case Node(s, "tensor_ones", (Backend.Const(sz:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(sz, Seq())(i => INT(1).x)
      t.x
    case Node(s, "tensor_consts", (Backend.Const(sz:Int))::(Backend.Const(n:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(sz, Seq())(i => INT(n).x)
      t.x
    case Node(s, "tensor_input", (Backend.Const(sz:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t1 = INPUT1(sz, Seq(s)) // necessary?
      t1.x
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
