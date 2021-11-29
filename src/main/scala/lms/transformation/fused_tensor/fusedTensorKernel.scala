package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorKernel extends Transformer {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  // import ArrayCPUTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._
  import CUDATypeLess._

  def gpu_array(size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC(size, m)
  }

  val tensors = new mutable.HashMap[Backend.Sym, (Node, List[Backend.Sym], Seq[Node])]

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(sz:Int)::Backend.Const(inputs:Seq[Backend.Sym])::(f@Backend.Block(arg::Nil, r, block, eff))::_, _) =>
      implicit val __pos = Adapter.oldSourceMap(s)
      val arr = CUDA_MALLOC(sz, manifest[Int])

      val tmp = CUDA_KERNEL3({xn: List[Backend.Exp] => 
        val array = (new ARRAY(xn(0))).withSrcType(__pos, manifest[Int])
        val value = (new NUM(xn(1))).withSrcType(__pos, manifest[Int])
        val size  = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

        val r1  = (new INT(r)).withSrcType(__pos, manifest[Int])

        val stride = gridDimX * blockDimX
        val tid = threadIdxX + blockIdxX * blockDimX
        for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int](size.x), Wrap[Int](stride.x))) {
          array(INT(Unwrap(i))) = r1; ()
        }
        Backend.Const(())
      }, manifest[Array[Int]], manifest[Int], manifest[Int])
      
      (tmp(arr, INT(0), INT(0), DIM3(0), DIM3(0))).x

    case Node(s, "tensor_show", Backend.Sym(x)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      Backend.Const(())
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
