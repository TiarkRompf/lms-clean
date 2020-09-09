package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.ArrayCPUOps
import lms.thirdparty.CUDATypeLess

import Backend._

// this transformation insert tensor communication (send/recv) ops when necessary
// Note: this class is still very rudimentary and only fixing tensor transportation locally
abstract class TensorResolvingDevice extends Transformer {

  import FixedSizeTensorDeviceTypeLess._
  import CUDATypeLess._

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::Backend.Const(d:Device)::
        (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = oldSourceMap(s)
      implicit val dd_ : Device = d

      val res_tensor = (new TENSOR(x)).to(d) + (new TENSOR(y).to(d))
      res_tensor.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = oldSourceMap(s)

      // FIXME(feiw): x is a Sym in the old graph, so we cannot use `Adapter.typeMap`
      // to get the type of x. However, the .to function is using the .et method of TENSOR
      // and it uses `Adapter.typeMap`.
      (new TENSOR(x)).to(CPU(0)).show
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
