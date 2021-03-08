package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorUnaryTypeLess extends FixedSizeDistributedTensorMutationTypeLess 
  with FixedSizeDistributedTensorBinaryTypeLess with FixedSizeDistributedTensorConvTypeLess {

  def Transpose(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    assert(tensor.shapeSize.size == 2, "input of transpose must be 2D")
    val res_tt = TensorType(Seq(tensor.resultType.shape(1), tensor.resultType.shape(0)), tensor.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_transpose", C(res_tt), C(anno), tensor.x)(tensor.x))).withSrcType(__pos, tensor.et)
  }

  def Negate(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_negate", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  def Invert(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_invert", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  def Tanh(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_tanh", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  def Relu(tensor: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = tensor.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_relu", C(res_tt), C(anno), tensor.x)(tensor.x)).withSrcType(__pos, tensor.et))
  }

  val unaryops = List("tensor_transpose", "tensor_negate", "tensor_invert", "tensor_tanh", "tensor_relu")

  override def mergable_dims(node: Node) = node match {
    case Node(s, op, _, _) if (unaryops.contains(op))=> List()
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: GradMapWrapper,
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "tensor_transpose", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node

        (() => {
          Accumulate(gradMap(a), Transpose(gradMap(s), anno), anno); ()
        }) +=: backwardNodes

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
          // val a_tensor = new TENSOR(transform(a))
          // val square = Mul(a_tensor, a_tensor, anno)
          // val grad = Negate(Div(gradMap(s), square, anno), anno)
          Accumulate(gradMap(a), InvertGrad(new TENSOR(transform(a)), gradMap(s), anno), anno); ()
        }) +=: backwardNodes
    
    case Node(s, "tensor_tanh", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node

        (() => {
          Accumulate(gradMap(a), TanhGrad(gradMap(s), new TENSOR(transform(s)), anno), anno); ()
        }) +=: backwardNodes
    
    case Node(s, "tensor_relu", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node

        (() => {
          Accumulate(gradMap(a), ReluGrad(gradMap(s), new TENSOR(transform(a)), anno), anno); ()
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

    def relu(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Relu(self, anno)
      Wrap[Tensor[T]](t.x)
    }

    // clipped relu
    def relu(threshold: Float, anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, threshold)
      val t = ActivationForward(self, p, "crelu", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def elu(alpha: Float, anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, alpha)
      val t = ActivationForward(self, p, "elu", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def sigmoid(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, 0.0f)
      val t = ActivationForward(self, p, "sigmoid", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }
    
    def cudnn_tanh(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, 0.0f)
      val t = ActivationForward(self, p, "tanh", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }
  }
}
