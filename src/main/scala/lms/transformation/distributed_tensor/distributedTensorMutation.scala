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
