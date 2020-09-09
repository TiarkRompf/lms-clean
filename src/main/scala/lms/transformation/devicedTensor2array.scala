package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.ArrayCPUTypeLess
import lms.thirdparty.{CudaOps, CUDATypeLess}

import Backend._


// lower Tensor computations to Array computations
// respect device allocation (simple distinction of GPU and CPU)
abstract class DevicedTensorLowering extends Transformer {

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeTensorDeviceTypeLess._
  import CUDATypeLess._

  // need a global mapping from Tensor to Array
  val t2a = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::(x:Backend.Sym)::_, _) =>
      t2a(s) = transform(x).asInstanceOf[Backend.Sym]
      Adapter.typeMap(transform(x)) = Adapter.oldTypeMap(x)
      s

    case Node(s, "tensor_sendrecv", Backend.Const(size:Seq[Int])::Backend.Const(td:Device)::Backend.Const(d:Device)::(x:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = oldSourceMap(s)
      val m = Adapter.oldTypeMap(s)
      val count = numeral(size)

      val res = ARRAYD(count, Adapter.oldTypeMap(s), td) // declare an array on device td
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      val kind = (d, td) match {
        case (CPU(_), CPU(_)) => HOST2HOST
        case (CPU(_), GPU(_)) => HOST2DEVICE
        case (GPU(_), CPU(_)) => DEVICE2HOST
        case (GPU(_), GPU(_)) => DEVICE2DEVICE
      }
      CUDA_MEMCOPY(res, new ARRAY(t2a(x)), count, kind, m)

      res.x

    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = oldSourceMap(s)

      val x_device = (new TENSOR(x)).device(graphCache)
      val y_device = (new TENSOR(y)).device(graphCache)
      assert(x_device == d && y_device == d)

      val res = ARRAYD(numeral(size), Adapter.oldTypeMap(s), d) // declare an array on device d
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      d match {
        case CPU(_) => ARRAY_ADD(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)) // calling the CPU version
        case GPU(_) => CUDA_ARRAY_ADD(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)); () // calling the GPU version
      }
      res.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = oldSourceMap(s)

      val shape = (new TENSOR(x)).shape(graphCache)
      val device = (new TENSOR(x)).device(graphCache)
      assert(device.isInstanceOf[CPU], "show_tensor must be on CPU")

      val arr = new ARRAY(t2a(x))

      ARRAY_PRINT(arr, INT(numeral(shape)))

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
