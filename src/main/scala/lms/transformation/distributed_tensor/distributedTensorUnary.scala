package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorUnaryTypeLess extends FixedSizeDistributedTensorMutationTypeLess {

  def Transpose(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    assert(tensor.shape_size.size == 2, "input of transpose must be 2D")
    val res_tt = TensorType(Seq(tensor.tensor_type.shape(1), tensor.tensor_type.shape(0)), tensor.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_transpose", C(res_tt), C(anno), tensor.x)(tensor.x))).withSrcType(__pos, tensor.et)
  }

  def Negate(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.tensor_type
    (new TENSOR(Adapter.g.reflectRead("tensor_negate", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  def Invert(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.tensor_type
    (new TENSOR(Adapter.g.reflectRead("tensor_invert", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  def Tanh(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.tensor_type
    (new TENSOR(Adapter.g.reflectRead("tensor_tanh", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "tensor_transpose", _, _) => List()
    case Node(s, "tensor_negate", _, _) => List()
    case Node(s, "tensor_invert", _, _) => List()
    case Node(s, "tensor_tanh", _, _) => List()
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: mutable.HashMap[Backend.Sym, TENSOR],
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "tensor_transpose", _, _) => ???

    case Node(s, "tensor_negate", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node

        (() => {
          Accumulate(gradMap(a), Negate(gradMap(s), anno), anno); ()
        }) +=: backwardNodes
     
    case Node(s, "tensor_invert", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node

        (() => {
          Accumulate(gradMap(a), Invert(gradMap(s), anno), anno); ()
        }) +=: backwardNodes
    
    case Node(s, "tensor_tanh", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node

        (() => {
          Accumulate(gradMap(a), Tanh(gradMap(s), anno), anno); ()
        }) +=: backwardNodes

    case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
  }
}


trait FixedSizeDistributedTensorOpsUnary extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsUnary[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def trans(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Transpose(self, anno)
      Wrap[Tensor[T]](t.x)
    }

    def neg(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Negate(self, anno)
      Wrap[Tensor[T]](t.x)
    }

    def inv(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Invert(self, anno)
      Wrap[Tensor[T]](t.x)
    }

    def tanh(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Tanh(self, anno)
      Wrap[Tensor[T]](t.x)
    }
  }
}
