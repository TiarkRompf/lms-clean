package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorLowering extends Transformer {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FusedTensorTypeLess._

  val tensors = new mutable.HashMap[Backend.Sym, (Node, List[Backend.Sym], Seq[Node])]

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_zeros", (Backend.Const(sz:Int))::(arr:Backend.Exp)::_, _) =>
      System.out.println(n)
      val array = new ARRAY(arr)
      val t = TENSOR(sz, array)(i => INT(0).x)
      t.x
    case Node(s, "tensor_ones", (Backend.Const(sz:Int))::(arr:Backend.Exp)::_, _) =>
      System.out.println(n)
      val array = new ARRAY(arr)
      val t = TENSOR(sz, array)(i => INT(1).x)
      t.x
    case Node(s, "tensor_add", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      System.out.println(n)
      val a = new TENSOR(x, useOldMetadata = true)
      val b = new TENSOR(y, useOldMetadata = true)
      val sz = a.size
      val array = ARRAY(10, manifest[Int])
      // val t = TENSOR(sz, array)(i => INT(a.apply(i)+ b.apply(i)).x)
      // val t = TENSOR(sz, array)(i => INT(-1).x)
      val x1 = a.apply(INT(0).x)
      val x2 = b.apply(INT(0).x)
      val r = x1 + x2
      System.out.println(r)
      val t = TENSOR(sz, array)(i => r.x)
      System.out.println(t)
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
