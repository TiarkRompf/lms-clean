package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorCommTypeLess extends FixedSizeDistributedTensorBaseTypeLess {

  def AllReduce(tensor: TENSOR)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectRead("tensor_allreduce", C(tensor.resultType), C(NAnno), tensor.x)(tensor.x))).withSrcType(__pos, tensor.et)
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "tensor_allreduce", _, _) => throw new Exception("should not have allreduce op in mergable dims")
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: GradMapWrapper,
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "tensor_allreduce", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        throw new Exception("should not have allreduce op in aircopCollect")

    case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
  }

  override def printTensor(node: Node, graph: Graph): String = node match {
    case Node(s, "tensor_allreduce", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
      s"$s = tensor_allreduce($a) (${symTensorShape(a, graph)})->${tt.toString}${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case _ => super.printTensor(node, graph)
  }
}
