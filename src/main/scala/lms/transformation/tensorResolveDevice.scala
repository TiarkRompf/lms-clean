package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess}

import Backend._

// this transformation insert tensor communication (send/recv) ops when necessary
// Note: this class is still very rudimentary and only fixing tensor transportation locally
abstract class TensorResolvingDevice extends Transformer {

  import FixedSizeTensorDeviceTypeLess._
  import CUDATypeLess._

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      implicit val dd_ : Device = d

      val res_tensor = (new TENSOR(transform(x))).to(d) + (new TENSOR(transform(y))).to(d)

      res_tensor.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = Adapter.oldSourceMap(s)

      (new TENSOR(transform(x))).to(CPU(0)).show
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
