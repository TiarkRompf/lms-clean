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
  import BaseTypeLess._

  def SEND(tensor: TENSOR, str:String, gflag:Boolean = false)(implicit __pos: SourceContext): UNIT = {
    val tag = if (gflag) str + "'" else str
    UNIT(Adapter.g.reflectEffect("tensor_send", C(tensor.resultType), C(tensor.annotation), C(tag), tensor.x)(tensor.x)(Adapter.CTRL))
  }

  def RECV(tensor: TENSOR, str:String, gflag:Boolean = false)(implicit __pos: SourceContext): TENSOR = {
    val tag = if (gflag) str + "'" else str
    (new TENSOR(Adapter.g.reflectWrite("tensor_recv", C(tensor.resultType), C(tensor.annotation), C(tag), tensor.x)(tensor.x, Adapter.CTRL))).withSrcType(__pos, tensor.et)
  }

  def AllReduce(tensor: TENSOR)(implicit __pos: SourceContext): TENSOR = {
    (new TENSOR(Adapter.g.reflectRead("tensor_allreduce", C(tensor.resultType), C(NAnno), tensor.x)(tensor.x))).withSrcType(__pos, tensor.et)
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "tensor_allreduce", _, _) => throw new Exception("should not have allreduce op in mergable dims")
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit], liftNodes: mutable.Set[Backend.Sym],
      gradMap: GradMapWrapper,
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "tensor_allreduce", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        throw new Exception("should not have allreduce op in aircopCollect")

    case Node(s, "tensor_module", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) => {
      val tag = Adapter.oldDefsCache(x) match {
        case Node(x, "module", Const(manno)::(b@Block(_,res,_,_))::_, _) => res.toString
        case _ => throw new Exception(s"$s is not tensor module")
      }
      forwardNodes += node
      (() => {
        implicit val pos = Adapter.oldSourceMap(s)
        SEND(gradMap(s),tag,true); ()
      }) +=: backwardNodes
    }

    case Node(s, "tensor_send", _, _) =>
        throw new Exception("shoculd not have send op in aircopCollect")

    case Node(s, "tensor_recv", _, _) =>
        throw new Exception("shoculd not have recv op in aircopCollect")

    case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, liftNodes, gradMap, momentumMap, transform)
  }

  override def printTensor(node: Node, graph: Graph): String = node match {
    case Node(s, "tensor_allreduce", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
      s"$s = tensor_allreduce($a) (${symTensorShape(a, graph)})->${tt.toString}${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case Node(s, "tensor_send", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) =>
      s"$s = tensor_send() -> tensor: ${x} ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}  Tag: $tag"
    case Node(s, "tensor_recv", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) =>
      s"$s = tensor_recv() -> tensor: ${x} ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}  Tag: $tag"
    case _ => super.printTensor(node, graph)
  }
}
