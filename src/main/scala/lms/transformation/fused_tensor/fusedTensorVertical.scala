package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
// import scala.collection.mutable.HashMap
// import scala.collection.immutable.Set


import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorVertical extends Transformer {

  override val name = "FusedTensorVerticalFusion"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import FusedTensorTypeLess._
  import CUDATypeLess._
  


  // map virtual tensors syms to node and context
  val tensors = new mutable.HashMap[Backend.Sym, (Node, List[Backend.Sym], Seq[Node])]

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(inputs:Seq[View])::_, _) =>
      tensors(s) = (n, path, inner)
      super.transform(n)
    case Node(s, "tensor_apply", (a:Backend.Sym)::(b:Backend.Exp)::_, _) if tensors.contains(a) =>
      val (Node(_, _, _::(f@Backend.Block(arg::Nil, r, block, eff))::_, _), path0, inner0) = tensors(a)
      try {
        subst(arg) = transform(b)
        withResetScope(path0, inner0) {
          traverse(f)
        }
        transform(r)
      } finally subst -= arg
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
