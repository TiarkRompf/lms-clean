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

  def MaskedFillForward[T](input: TENSOR, mask: TENSOR, value: T, anno: Anno, __pos: SourceContext): TENSOR = {
    require(input.shapeSize == mask.shapeSize, "shape of mask must be equal to shape of the tensor")
    val res_tt = input.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_maskedfill", C(res_tt), C(anno), input.x, mask.x,
      C(value))(input.x, mask.x)).withSrcType(__pos, input.et))
  }

  def MaskedFillBackward(doutput: TENSOR, mask: TENSOR, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = doutput.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_maskedfill_bwd", C(res_tt), C(anno), doutput.x, mask.x)
      (doutput.x, mask.x)).withSrcType(__pos, doutput.et))
  }

  def LogSoftmaxForward(input: TENSOR, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = input.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_logsoftmax", C(res_tt), C(anno), input.x)(input.x))
      .withSrcType(__pos, input.et))
  }

  def LogSoftmaxBackward(output: TENSOR, doutput: TENSOR, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = output.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_logsoftmax_bwd", C(res_tt), C(anno), output.x, doutput.x)(output.x, doutput.x))
      .withSrcType(__pos, output.et))
  }

  def Permute(input: TENSOR, perm: List[Int], anno: Anno, __pos: SourceContext): TENSOR = {
    require(perm.sortWith(_ < _) == List.range(0, input.shapeSize.size), "dims must be a valid permutation of input dimensions")
    val res_tt = TensorType(perm.map(input.resultType.shape(_)), input.resultType.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_permute", C(res_tt), C(anno), input.x, C(perm))(input.x))
      .withSrcType(__pos, input.et))
  }

  def EmbeddingForward(input: TENSOR, indices: TENSOR, anno: Anno, __pos: SourceContext): TENSOR = {
    require(input.shapeSize.size == 2, "input must be a 2-D matrix")
    require(indices.shapeSize.size == 1, "indices must be an 1-D array")
    System.out.println("fwd pass")
    System.out.println("input: " + input.resultType.shape)
    System.out.println("indices: " + indices.resultType.shape)
    val res_tt = TensorType(Seq(indices.resultType.shape(0), input.resultType.shape(1)), input.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_embedding", C(res_tt), C(anno), input.x, indices.x)
      (input.x, indices.x)).withSrcType(__pos, input.et))
  }

  def EmbeddingBackward(input: TENSOR, doutput: TENSOR, indices: TENSOR, anno: Anno, __pos: SourceContext): TENSOR = {
    System.out.println("bwd pass")
    System.out.println("input: " + input.resultType.shape)
    System.out.println("doutput: " + doutput.resultType.shape)
    System.out.println("indices: " + indices.resultType.shape)
    val res_tt = TensorType(Seq(input.resultType.shape(0), doutput.resultType.shape(1)), doutput.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_embedding_bwd", C(res_tt), C(anno), input.x, doutput.x, indices.x)
      (doutput.x, indices.x)).withSrcType(__pos, doutput.et))
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "tensor_softmax", _, _) => List()
    case Node(s, "tensor_maskedfill", tt::anno::(input:Backend.Sym)::(mask:Backend.Sym)::_, _) =>
      val input_type = (new TENSOR(input, useOldMetadata=true)).resultType
      val mask_type = (new TENSOR(mask, useOldMetadata=true)).resultType
      (input_type.shape.reverse zip mask_type.shape.reverse).toList map { case (a:Size, b:Size) => (a.dim, b.dim)}
    case Node(s, "tensor_logsoftmax", _, _) => List()
    case Node(s, "tensor_permute", _, _) => List()
    case Node(s, "tensor_embedding", tt::anno::(input:Backend.Sym)::(mask:Backend.Sym)::_, _) => // todo: fixme
      /*
      val input_type = (new TENSOR(input, useOldMetadata=true)).resultType
      val mask_type = (new TENSOR(mask, useOldMetadata=true)).resultType
      (input_type.shape.reverse zip mask_type.shape.reverse).toList map { case (a:Size, b:Size) => (a.dim, b.dim)}*/
      List()
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
      case Node(s, "tensor_maskedfill", tt::Backend.Const(anno:Anno)::(input:Backend.Sym)::(mask:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val grad = MaskedFillBackward(gradMap(s), new TENSOR(transform(mask)), anno, pos)
            Accumulate(gradMap(input), grad, anno); ()
        }) +=: backwardNodes
      case Node(s, "tensor_logsoftmax", tt::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val x = new TENSOR(transform(s))
            val grad = LogSoftmaxBackward(x, gradMap(s), anno, pos)
            Accumulate(gradMap(input), grad, anno); ()
        }) +=: backwardNodes
      case Node(s, "tensor_permute", tt::Backend.Const(anno:Anno)::(input:Backend.Sym)::Backend.Const(dims:List[Int])::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val grad = Permute(gradMap(s), dims, anno, pos)
            Accumulate(gradMap(input), grad, anno); ()
        }) +=: backwardNodes
      case Node(s, "tensor_embedding", tt::Backend.Const(anno:Anno)::(input:Backend.Sym)::(indices:Backend.Sym)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val grad = EmbeddingBackward(new TENSOR(transform(input)), gradMap(s), new TENSOR(transform(indices)), anno, pos)
            Accumulate(gradMap(input), grad, anno); ()
        }) +=: backwardNodes

      case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
    }

  override def printTensor(node: Node, graph: Graph): String = node match {
    case Node(s, "tensor_softmax", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::_, _) =>
      s"$s = tensor_softmax($a) (${symTensorShape(a, graph)})->${tt.toString}${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case Node(s, "tensor_maskedfill", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::(mask:Backend.Sym)::_, _) =>
      s"$s = tensor_maskedfill($input, $mask) (${symTensorShape(input, graph)}, ${symTensorShape(mask, graph)})->${tt.toString}${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case Node(s, "tensor_logsoftmax", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
      s"$s = tensor_logsoftmax($input) (${symTensorShape(input, graph)})->${tt.toString}${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case Node(s, "tensor_permute", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::Backend.Const(dims:List[Int])::_, _) =>
      s"$s = tensor_permute($input, permutation=${dims}) (${symTensorShape(input, graph)})->${tt.toString}${if (anno != NAnno) s"\nAnno: $anno" else ""}"
    case _ => super.printTensor(node, graph)
  }
}

trait FixedSizeDistributedTensorOpsMisc extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._
  import scala.collection.immutable.Seq

  implicit class TensorOpsMisc[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    val softmax_params_def = SoftmaxParam(1.0f, 0.0f)
    def softmax(params: SoftmaxParam = softmax_params_def)(implicit __pos: SourceContext, anno: Anno): Rep[Tensor[T]] = {
      val t = SoftmaxForward(self, params, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def softmax(params: SoftmaxParam, anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = SoftmaxForward(self, params, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def maskedFill(mask: Rep[Tensor[Int]], value: Float)(implicit __pos: SourceContext, anno: Anno): Rep[Tensor[T]] = {
      val t = MaskedFillForward(self, tensor(mask), value, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def logSoftmax(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = LogSoftmaxForward(self, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def logSoftmax(implicit __pos: SourceContext, anno: Anno): Rep[Tensor[T]] = {
      val t = LogSoftmaxForward(self, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def permute(perm: List[Int])(implicit __pos: SourceContext, anno: Anno): Rep[Tensor[T]] = {
      val t = Permute(self, perm, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def permute(perm: List[Int], anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Permute(self, perm, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def embedding(indices: Rep[Tensor[Int]])(implicit __pos: SourceContext, anno: Anno): Rep[Tensor[T]] = {
      val t = EmbeddingForward(self, tensor(indices), anno, __pos)
      Wrap[Tensor[T]](t.x)
    }
  }
}
