package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorMutationTypeLess extends FixedSizeDistributedTensorBaseTypeLess {
  import BaseTypeLess._

  def Accumulate(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): UNIT = {
    UNIT(Adapter.g.reflectEffect("accum_tensor", C(anno), x.x, y.x)(x.x, y.x)(x.x))
  }

  def Optimize(x: TENSOR, grad: TENSOR, momentum: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): UNIT = {
    UNIT(Adapter.g.reflectEffect("optimize_tensor", C(anno), x.x, grad.x, momentum.x)(x.x, grad.x, momentum.x)(x.x))
  }

  def AllReduceInPlace(x: TENSOR, devices: Seq[Device], mode: String)(implicit __pos: SourceContext): UNIT = {
    UNIT(Adapter.g.reflectEffect("all_reduce_tensor", C(devices), x.x, C(mode))(x.x)(x.x))
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "accum_tensor", anno::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).resultType
      val y_type = (new TENSOR(y, useOldMetadata=true)).resultType
      (x_type.shape zip y_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
    case Node(s, "optimizer_tensor", anno::(x:Backend.Sym)::(g:Backend.Sym)::(m:Backend.Sym)::_, _) =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).resultType
      val g_type = (new TENSOR(g, useOldMetadata=true)).resultType
      val m_type = (new TENSOR(m, useOldMetadata=true)).resultType
      val mergable_a = (x_type.shape zip g_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
      val mergable_b = (g_type.shape zip m_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
      mergable_a ++ mergable_b
    case Node(s, "all_reduce_tensor", _, _) => List()
    case _ => super.mergable_dims(node)
  }

  override def printTensor(node: Node, graph: Graph): String = node match {
    case Node(s, "accum_tensor", Backend.Const(anno:Anno)::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      s"$s = accum_tensor($x, $y) (${symTensorShape(x, graph)}, ${symTensorShape(y, graph)})${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case Node(s, "optimize_tensor", Backend.Const(anno:Anno)::(x:Backend.Sym)::(g:Backend.Sym)::(m:Backend.Sym)::_, _) =>
      s"$s = optimize_tensor($x, $g, $m) (${symTensorShape(x, graph)}, ${symTensorShape(g, graph)}, ${symTensorShape(m, graph)})${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case Node(s, "all_reduce_tensor", Backend.Const(devices:Seq[Device])::(x:Backend.Sym)::Backend.Const(mode:String)::_, _) =>
      s"$s = all_reduce_tensor($x, mode=$mode, devices=$devices) (${symTensorShape(x, graph)})"
    case _ => super.printTensor(node, graph)
  }
}


trait FixedSizeDistributedTensorOpsMutation extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsMutation[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def += (y: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Unit] = {
      Accumulate(self, tensor(y), anno); ()
    }

    def optimize(grad: Rep[Tensor[T]], momentum: Rep[Tensor[T]], anno: Anno = NAnno)(implicit __pos: SourceContext): Rep[Unit] = {
      Optimize(self, tensor(grad), tensor(momentum), anno); ()
    }

    def allReduceInPlace(devices:Seq[Device], mode:String)(implicit pos: SourceContext): Rep[Unit] = {
      AllReduceInPlace(self, devices, mode); ()
    }
  }
}
