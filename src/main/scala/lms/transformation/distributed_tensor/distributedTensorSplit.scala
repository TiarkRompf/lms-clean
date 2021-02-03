package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorSplitTypeLess extends FixedSizeDistributedTensorMutationTypeLess {
  import BaseTypeLess._

  class SPLIT_OP(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends OPERATION(x, useOldMetadata) {
    val axis: Int = gc.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, "op_split", tts::anno::o::Backend.Const(axis:Int)::_, _)) => axis
      case a => throw new Exception(s"Node $a is not a SplitOp")
    }
  }

  def SplitOp(x: TENSOR, axis: Int, slices: List[Int], anno: Anno = NAnno)(implicit __pos: SourceContext): SPLIT_OP = {
    val x_tensor_type = x.tensor_type
    val x_tensor_shape = x_tensor_type.shape
    assert(slices.sum == x_tensor_shape(axis).size)
    val result_tensor_shapes = slices map { s =>
      // FIXME(feiw) we asked the split result to use the same dim at the split axis. is this correct?
      x_tensor_shape.zipWithIndex.map {
        case(a, i) => if (i == axis) Size(a.dim, s) else a
      }
    }
    val result_tensor_types = result_tensor_shapes map { s =>
      TensorType(s, x_tensor_type.et, anno)
    }
    new SPLIT_OP(Adapter.g.reflectRead("op_split", C(result_tensor_types), C(anno), x.x, C(axis))(x.x)).withSrcType(__pos, x.et)
  }

  def Concat(xs: List[TENSOR], axis: Int, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    // FIXME(feiw) assert shape and element type
    val concatSize = xs.map(_.tensor_type).map(_.shape).map(s=>s(axis)).map(_.size).sum
    val concatShape = xs(0).tensor_type.shape.zipWithIndex.map {
      case(s, i) => if (i == axis) Size(s.dim, concatSize) else s
    }
    val concatType = TensorType(concatShape, xs(0).et, anno)
    val defs = xs.map(_.x).toSeq
    val all_defs = (C(concatType)::C(anno)::xs.map(_.x)).toSeq
    (new TENSOR(Adapter.g.reflectRead("tensor_concat", all_defs:_*)(defs:_*))).withSrcType(__pos, xs(0).et)
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "op_split", _, _) => List()
    case Node(s, "tensor_concat", _, _) => List() // FIXME(feiw) add!
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: mutable.HashMap[Backend.Sym, TENSOR],
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "op_split", tts::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      // save forward op in forwardNodes
      forwardNodes += node
      // save backward op in backwardNodes
      (() => {
        val splitOp = new SPLIT_OP(s, useOldMetadata=true)
        val grads = splitOp.getResults.map(x => gradMap(x.x.asInstanceOf[Backend.Sym]))
        val c_grads = Concat(grads, splitOp.axis, anno)
        Accumulate(gradMap(x), c_grads, anno); ()
      }) +=: backwardNodes

    case _ =>
      super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
  }

}


trait FixedSizeDistributedTensorOpsSplit extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsSplit[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def split(axis: Int, slices: List[Int], anno: Anno = NAnno)(implicit __pos: SourceContext): List[Rep[Tensor[T]]] = {
      val op = SplitOp(self, axis, slices, anno)
      op.getResults.map(x => Wrap[Tensor[T]](x.x))
    }
  }
}

