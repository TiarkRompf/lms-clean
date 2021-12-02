package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorToCuda extends Transformer {

  override val name = "FusedTensorToCuda"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._
  import CUDATypeLess._

  def gpu_array(size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC(size, m)
  }


  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(sz:Seq[Int])::Backend.Const(inputs:Seq[Backend.Sym])::(f@Backend.Block(arg::Nil, r, block, eff))::_, _) =>
      implicit val __pos = Adapter.oldSourceMap(s)
      val sz1 = sz.sum

      val arr = new ARRAY(inputs.head) // for now, assume only one input
      // System.out.println("input: " + inputs.head)
      // System.out.println("arr: " + arr)
      // System.out.println("res:" + r)

      val kernel = CUDA_KERNEL3({ xn: List[Backend.Exp] => 
        val array = (new ARRAY(xn(0))).withSrcType(__pos, manifest[Int])
        val value = (new NUM(xn(1))).withSrcType(__pos, manifest[Int]) // not used
        val size  = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

        val stride = gridDimX * blockDimX
        val tid = threadIdxX + blockIdxX * blockDimX

        val i = var_new(Wrap[Int](tid.x))

        // System.out.println("i: " + UnwrapV(i))
        // PRINTF("%d", INT(Unwrap(readVar(i))))
        
        __whileDo(ordering_lt(readVar(i), Wrap[Int](size.x)), {
          // replace input to function argument, tensor lambda to loop index
          try {
            subst(inputs.head) = array.x
            subst(arg) = UnwrapV(i)
            traverse(f)
          } finally {
            subst -= inputs.head
            subst -= arg
          }
          array(INT(Unwrap(readVar(i)))) = INT(transform(r))
          i += Wrap[Int](stride.x)
        })

        Backend.Const(())
      }, manifest[Array[Int]], manifest[Int], manifest[Int])
      
      (kernel(arr, INT(0), INT(sz1), DIM3(0), DIM3(0))).x

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
