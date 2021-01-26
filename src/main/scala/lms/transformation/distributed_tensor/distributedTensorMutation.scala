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

  override def mergable_dims(node: Node) = node match {
    case Node(s, "accum_tensor", anno::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).tensor_type
      val y_type = (new TENSOR(y, useOldMetadata=true)).tensor_type
      (x_type.shape zip y_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
    case Node(s, "optimizer_tensor", anno::(x:Backend.Sym)::(g:Backend.Sym)::(m:Backend.Sym)::_, _) =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).tensor_type
      val g_type = (new TENSOR(g, useOldMetadata=true)).tensor_type
      val m_type = (new TENSOR(m, useOldMetadata=true)).tensor_type
      val mergable_a = (x_type.shape zip g_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
      val mergable_b = (g_type.shape zip m_type.shape).toList map { case (a:Size, b:Size) => (a.dim, b.dim) }
      mergable_a ++ mergable_b
    case _ => super.mergable_dims(node)
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
  }
}
