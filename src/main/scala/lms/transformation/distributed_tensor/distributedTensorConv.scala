package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

// currently, stride = 1, padding = 0
trait FixedSizeDistributedTensorConvTypeLess extends FixedSizeDistributedTensorMutationTypeLess {
  /*
  def Conv(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val weight_size = x.shape_size
    val kernel_size = y.shape_size
    assert(kernel_size(0) == kernel_size(1)) // kernel should be square
    val res_tt = (weight_size(0) - kernel_size(0), weight_size(1) - kernel_size(1))
    (new TENSOR(Adapter.reflectRead("tensor_conv", C(res_tt), C(anno), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }*/

  
  
  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
    weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
    gradMap: mutable.HashMap[Backend.Sym, TENSOR],
    momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
    transform: Backend.Exp => Backend.Exp) = node match {
      case Node(s, "tensor_conv", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        // save forward op in forwardNodes
        forwardNodes += node
        // save backward op in backwardNodes
        (() => {
          ()
        }) +=: backwardNodes
        (() => {
          ()
        }) +=: backwardNodes

      case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
    }
}
