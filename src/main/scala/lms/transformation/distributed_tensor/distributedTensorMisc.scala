package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}
import lms.thirdparty.{CUDNNTypeLess, CUDNNOps}
import lms.transformation.util.CudnnUtils

import Backend._

trait FixedSizeDistributedTensorMiscTypeLess extends FixedSizeDistributedTensorMutationTypeLess with CudnnUtils {
  import BaseTypeLess._

  def SoftmaxForward(input: TENSOR, params: SoftmaxParam, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = input.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_softmax", C(res_tt), C(anno), input.x, 
      C(params))(input.x)).withSrcType(__pos, input.et))
  }

  def SoftmaxBackward(output: TENSOR, doutput: TENSOR, params: SoftmaxParam, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = doutput.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_softmax_bwd", C(res_tt), C(anno), output.x, doutput.x, 
      C(params))(output.x, doutput.x)).withSrcType(__pos, doutput.et))
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "tensor_softmax", _, _) => List()
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
    weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
    gradMap: GradMapWrapper,
    momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
    transform: Backend.Exp => Backend.Exp) = node match {
      case Node(s, "tensor_softmax", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params:SoftmaxParam)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val x = new TENSOR(transform(s))
            val grad = SoftmaxBackward(x, gradMap(s), params, anno, pos)
            Accumulate(gradMap(a), grad, anno); ()
        }) +=: backwardNodes

      case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
    }
}


trait FixedSizeDistributedTensorOpsMisc extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._
  import scala.collection.immutable.Seq

  implicit class TensorOpsMisc[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)
    
    val softmax_params_def = SoftmaxParam(1.0f, 0.0f)
    def softmax(anno: Anno, params: SoftmaxParam = softmax_params_def)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = SoftmaxForward(self, params, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }
  }
}
