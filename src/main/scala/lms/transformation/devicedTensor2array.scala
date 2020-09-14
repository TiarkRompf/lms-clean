package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._


// lower Tensor computations to Array computations
// respect device allocation (simple distinction of GPU and CPU)
abstract class DevicedTensorLowering extends Transformer {

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeTensorDeviceTypeLess._
  import CUDATypeLess._
  import CUBLASTypeLess._

  // need a global mapping from Tensor to Array
  val t2a = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::(x:Backend.Sym)::_, _) =>
      t2a(s) = transform(x).asInstanceOf[Backend.Sym]
      Adapter.typeMap(transform(x)) = Adapter.oldTypeMap(x)
      s

    case Node(s, "tensor_sendrecv", Backend.Const(size:Seq[Int])::Backend.Const(td:Device)::Backend.Const(d:Device)::(x:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
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
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)

      val x_device = (new TENSOR(x, old = true)).device
      val y_device = (new TENSOR(y, old = true)).device
      assert(x_device == d && y_device == d)

      val res = ARRAYD(numeral(size), Adapter.oldTypeMap(s), d) // declare an array on device d
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      d match {
        case CPU(_) => ARRAY_ADD(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)) // calling the CPU version
        case GPU(_) => CUDA_ARRAY_ADD(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)); () // calling the GPU version
      }
      res.x

    case Node(s, "tensor_minus", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)

      val x_device = (new TENSOR(x, old = true)).device
      val y_device = (new TENSOR(y, old = true)).device
      assert(x_device == d && y_device == d)

      val res = ARRAYD(numeral(size), Adapter.oldTypeMap(s), d) // declare an array on device d
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      d match {
        case CPU(_) => ARRAY_MINUS(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)) // calling the CPU version
        case GPU(_) => CUDA_ARRAY_MINUS(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)); () // calling the GPU version
      }
      res.x

    case Node(s, "tensor_mult", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)

      val x_device = (new TENSOR(x, old = true)).device
      val y_device = (new TENSOR(y, old = true)).device
      assert(x_device == d && y_device == d)

      val res = ARRAYD(numeral(size), Adapter.oldTypeMap(s), d) // declare an array on device d
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      d match {
        case CPU(_) => ARRAY_MULT(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)) // calling the CPU version
        case GPU(_) => CUDA_ARRAY_MULT(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)); () // calling the GPU version
      }
      res.x

    case Node(s, "tensor_div", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)

      val x_device = (new TENSOR(x, old = true)).device
      val y_device = (new TENSOR(y, old = true)).device
      assert(x_device == d && y_device == d)

      val res = ARRAYD(numeral(size), Adapter.oldTypeMap(s), d) // declare an array on device d
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      d match {
        case CPU(_) => ARRAY_DIV(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)) // calling the CPU version
        case GPU(_) => CUDA_ARRAY_DIV(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, numeral(size)); () // calling the GPU version
      }
      res.x

    case Node(s, "tensor_dot", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)

      // FIXME(feiw) something can be fixed here
      val (x_device, x_shape) = (new TENSOR(transform(x))).device_shape
      val (y_device, y_shape) = (new TENSOR(transform(y))).device_shape
      assert(x_device == d && y_device == d)

      val res = ARRAYD(numeral(size), Adapter.oldTypeMap(s), d) // declare an array on device d
      t2a(s) = res.x.asInstanceOf[Backend.Sym]

      d match {
        case CPU(_) => if (x_shape.size == 1 && y_shape.size == 1) {
          ARRAY_VVDOT(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, INT(x_shape(0)))
        } else if (x_shape.size == 2 && y_shape.size == 1) {
          ARRAY_MVDOT(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, INT(x_shape(0)), INT(x_shape(1)))
        } else if (x_shape.size == 2 && y_shape.size == 2) {
          ARRAY_MMDOT(new ARRAY(t2a(x)), new ARRAY(t2a(y)), res, INT(x_shape(0)), INT(x_shape(1)), INT(y_shape(1)))
        } else {
          throw new Exception("dot for higher than 2D is not yet supported")
        }

        case GPU(_) => if (x_shape.size == 1 && y_shape.size == 1) {
          // FIXME(feiw) maybe this res must be on CPU because otherwise it segfault?
          CUBLAS_SDOT(CUBLAS_HANDLE, INT(x_shape(0)), new ARRAY(t2a(x)), 1, new ARRAY(t2a(y)), 1, res)
        } else if (x_shape.size == 2 && y_shape.size == 1) {
          val m = INT(x_shape(0))
          val n = INT(x_shape(1))
          CUBLAS_SGEMV(CUBLAS_HANDLE, CUBLAS_OP_T, n, m, ONE, new ARRAY(t2a(x)), n, new ARRAY(t2a(y)), 1, ZERO, res, 1)
        } else if (x_shape.size == 2 && y_shape.size == 2) {
          val m = INT(x_shape(0))
          val n = INT(y_shape(1))
          val k = INT(x_shape(1))
          CUBLAS_SGEMM(CUBLAS_HANDLE, CUBLAS_OP_N, CUBLAS_OP_N, n, m, k, ONE, new ARRAY(t2a(y)), n, new ARRAY(t2a(x)),
            k, ZERO, res, n)
        } else {
          throw new Exception("dot for higher than 2D is not yet supported")
        }
      }
      res.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = Adapter.oldSourceMap(s)

      val shape = (new TENSOR(x, old = true)).shape
      val device = (new TENSOR(x, old = true)).device
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
