package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.HashMap
import scala.collection.immutable.Set

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}
import lms.thirdparty.{CLibTypeLess}

import Backend._

abstract class FusedTensorToCuda extends Transformer {

  override val name = "FusedTensorToCuda"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._
  import CUDATypeLess._
  import CLibTypeLess._

  def gpu_array(size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC(size, m)
  }

  def ScanFile(scan: ARRAY, count: INT, filenameFormat: Rep[String], filenameArgs: Rep[Any]*)(implicit pos: SourceContext) = {
    val function = scan.et match {
      case m if m == manifest[Float] => "scan_float_array"
      case m if m == manifest[Int] => "scan_int_array"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, scan.x::count.x::Unwrap(filenameFormat)::filenameArgs.map(Unwrap).toList:_*)(Seq[Int](), Seq[Int](0,1), Set[Int]())
  }

    // map concrete (input) tensors to CUDA arrays
  val tensor2arr = new mutable.HashMap[Backend.Sym, Backend.Exp]

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_input", Backend.Const(inputs:Seq[View])::_, _) =>
      implicit val __pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(s, useOldMetadata = true)
      val size = t.size
      val m = manifest[Int]
      val cpuArray = ARRAY(size, m)
      ScanFile(cpuArray, size, unit("input"))
      CUDA_SET_DEVICE(INT(0))
      val gpuArray = CUDA_MALLOC(size, m) // allocate CUDA array for input tensors
      CUDA_MEMCPY(gpuArray, cpuArray, size, HOST2DEVICE, m)

      tensor2arr(s) = gpuArray.x
      gpuArray.x

    case Node(s, "tensor_apply", (a:Backend.Sym)::(b:Backend.Exp)::_, _) if tensor2arr.contains(a) =>
      implicit val __pos = Adapter.oldSourceMap(s)
      val arr = new ARRAY(tensor2arr(a))
      (arr.apply(INT(transform(b)))).x // change tensor apply to array apply

    case Node(s, "tensor", Backend.Const(inputs:Seq[View])::(f@Backend.Block(arg::Nil, r, block, eff))::_, _) =>
      implicit val __pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(s, useOldMetadata = true)
      val sz1 = t.size

      // input array. for now, assume only one input
      val in_arr = new ARRAY(tensor2arr(inputs.head.t))
      // System.out.println("in_arr:" + in_arr)
      // allocate output array. assume only one output
      val out_arr = CUDA_MALLOC(sz1, manifest[Int])
      // System.out.println("in_arr:" + in_arr)
      // System.out.println("out_arr:" + out_arr)

      val kernel = CUDA_KERNEL3({ xn: List[Backend.Exp] => 
        val in_array = (new ARRAY(xn(0))).withSrcType(__pos, manifest[Int])
        val out_array = (new ARRAY(xn(1))).withSrcType(__pos, manifest[Int])
        val size  = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

        val stride = gridDimX * blockDimX
        val tid = threadIdxX + blockIdxX * blockDimX

        val i = var_new(Wrap[Int](tid.x))
        
        __whileDo(ordering_lt(readVar(i), Wrap[Int](size.x)), {
          // replace input to function argument, tensor lambda to loop index
          try {
            subst(inputs.head.t) = in_array.x
            subst(arg) = Unwrap(readVar(i))
            traverse(f)
          } finally {
            subst -= inputs.head.t
            subst -= arg
          }
          out_array(INT(Unwrap(readVar(i)))) = INT(transform(r))
          i += Wrap[Int](stride.x)
        })

        Backend.Const(())
      }, manifest[Array[Int]], manifest[Array[Int]], manifest[Int])
      
      (kernel(in_arr, out_arr, INT(sz1), DIM3(0), DIM3(0))).x


    case Node(s, "tensor_show", (x:Backend.Sym)::_, _) =>
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
