package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorBinaryTypeLess extends FixedSizeDistributedTensorMutationTypeLess {

  def ElemWiseNoBroadCasting(x: TENSOR, y: TENSOR, anno: Anno, __pos: SourceContext)(op: String): TENSOR = {
    assert(x.shape_size == y.shape_size)
    assert(x.et == y.et)
    (new TENSOR(Adapter.g.reflectRead(op, C(x.tensor_type), C(anno), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }

  def Add(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_add")

  def Sub(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_minus")

  def Mul(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_mult")

  def Div(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR =
    ElemWiseNoBroadCasting(x, y, anno, __pos)("tensor_div")

  override def mergable_dims(node: Node) = node match {
    case Node(s, op, tt::anno::(x:Backend.Sym)::(y:Backend.Sym)::_, _)
        if (op == "tensor_add" || op == "tensor_minus" || op == "tensor_mult" || op == "tensor_div") =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).tensor_type
      val y_type = (new TENSOR(y, useOldMetadata=true)).tensor_type
      (x_type.shape zip y_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      grad_map: mutable.HashMap[Backend.Sym, TENSOR],
      momentum_map: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "tensor_add", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      // save forward op in forwardNodes
      forwardNodes += node
      // save backward op in backwardNodes
      (() => {
        Accumulate(grad_map(a), grad_map(s), anno); ()
      }) +=: backwardNodes
      (() => {
        Accumulate(grad_map(b), grad_map(s), anno); ()
      }) +=: backwardNodes

    // case Node(s, "tensor_sub", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
    //   implicit val pos = Adapter.oldSourceMap(s)
    //   // save forward op in forwardNodes
    //   forwardNodes += node
    //   // save backward op in backwardNodes
    //   (() => {
    //     Accumulate(grad_map(a), grad_map(s), anno); ()
    //   }) +=: backwardNodes
    //   (() => {
    //     val b_grad = Neg(grad_map(s), anno)
    //     Accumulate(grad_map(b), b_grad, anno); ()
    //   }) +=: backwardNodes

    case Node(s, "tensor_mult", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      // save forward op in forwardNodes
      forwardNodes += node
      // save backward op in backwardNodes
      (() => {
        val a_tensor = new TENSOR(transform(a))
        val b_grad = Mul(a_tensor, grad_map(s), anno)
        Accumulate(grad_map(b), b_grad, anno); ()
      }) +=: backwardNodes
      (() => {
        val b_tensor = new TENSOR(transform(b))
        val a_grad = Mul(b_tensor, grad_map(s), anno)
        Accumulate(grad_map(a), a_grad, anno); ()
      }) +=: backwardNodes

    case Node(s, "tensor_div", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      // save forward op in forwardNodes
      forwardNodes += node
      // save backward op in backwardNodes
      (() => {
        ???
      }) +=: backwardNodes
      (() => {
        ???
      }) +=: backwardNodes

    case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, grad_map, momentum_map, transform)
  }
}


trait FixedSizeDistributedTensorOpsBinary extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsBinary[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    // def + (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.+(y, anno)
    def + (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Add(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def - (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.-(y, anno)
    def - (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Sub(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def * (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this.*(y, anno)
    def * (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Mul(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    // def / (y: Rep[Tensor[T]])(implicit anno: Anno, __pos: SourceContext): Rep[Tensor[T]] = this./(y, anno)
    def / (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Div(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }
  }
}

