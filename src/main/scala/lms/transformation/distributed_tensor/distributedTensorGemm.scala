package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUOps, CUDATypeLess, CudaOps}

import Backend._

trait FixedSizeDistributedTensorGemmTypeLess extends FixedSizeDistributedTensorMutationTypeLess {

  def Dot(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = (x.shapeSize.size, y.shapeSize.size) match {
      case (1,1) => // vector-vector-dot
        assert(x.shapeSize == y.shapeSize)
        TensorType(Seq(Size(Dim(next_dim_name), 1)), x.et)
      case (2,1) => // matrix-vector-dot
        assert(x.shapeSize(1) == y.shapeSize(0))
        TensorType(x.resultType.shape.take(1), x.et)
      case (2,2) => // matrix-matrix-dot
        assert(x.shapeSize(1) == y.shapeSize(0))
        TensorType(Seq(x.resultType.shape(0), y.resultType.shape(1)), x.et)
      case _ => throw new Exception("not yet supporting high dimension dot")
    }
    assert(x.et == y.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_dot", C(res_tt), C(anno), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }

  def DotWithTranspose(x: TENSOR, y: TENSOR, anno: Anno = NAnno, transL: Boolean = false, transR: Boolean = false)(implicit __pos: SourceContext): TENSOR = {
    val res_tt = (x.shapeSize.size, y.shapeSize.size) match {
      case (1,1) => // vector-vector-dot
        assert(!transL && !transR, "cannot transpose the operand in vector-vector-dot")
        assert(x.shapeSize == y.shapeSize)
        TensorType(Seq(Size(Dim(next_dim_name), 1)), x.et)
      case (2,1) => // matrix-vector-dot
        assert(!transL && !transR, "cannot transpose the operand in matrix-vector-dot for now")
        assert(x.shapeSize(1) == y.shapeSize(0))
        TensorType(x.resultType.shape.take(1), x.et)
      case (2,2) => // matrix-matrix-dot
        val left_check = if (transL) x.shapeSize(0) else x.shapeSize(1)
        val left_dim = if (transL) x.resultType.shape(1) else x.resultType.shape(0)
        val right_check = if (transR) x.shapeSize(1) else x.shapeSize(0)
        val right_dim = if (transR) y.resultType.shape(0) else y.resultType.shape(1)
        assert(left_check == right_check)
        TensorType(Seq(left_dim, right_dim), x.et)
      case _ => throw new Exception("not yet supporting high dimension dot")
    }
    assert (x.et == y.et)
    (new TENSOR(Adapter.g.reflectRead("tensor_dot_with_transpose", C(res_tt), C(anno), C(transL), C(transR), x.x, y.x)(x.x, y.x))).withSrcType(__pos, x.et)
  }

  override def mergable_dims(node: Node) = node match {
    case Node(s, "tensor_dot", tt::anno::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).resultType
      val y_type = (new TENSOR(y, useOldMetadata=true)).resultType
      (x_type.shape.size, y_type.shape.size) match {
        case (1,1) => List((x_type.shape.head.dim, y_type.shape.head.dim))
        case (2,1) => List((x_type.shape.last.dim, y_type.shape.head.dim))
        case (2,2) => List((x_type.shape.last.dim, y_type.shape.head.dim))
        case r => throw new Exception(s"not yet handling ranks $r")
      }
    case Node(s, "tensor_dot_with_transpose", tt::anno::Backend.Const(transL)::Backend.Const(transR)::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      val x_type = (new TENSOR(x, useOldMetadata=true)).resultType
      val y_type = (new TENSOR(y, useOldMetadata=true)).resultType
      (x_type.shape.size, y_type.shape.size, transL, transR) match {
        case (1, 1, false, false) => List((x_type.shape.head.dim, y_type.shape.head.dim))
        case (2, 1, false, false) => List((x_type.shape.last.dim, y_type.shape.head.dim))
        case (2, 1, true, false) => List((x_type.shape.head.dim, y_type.shape.head.dim))
        case (2, 2, false, false) => List((x_type.shape.last.dim, y_type.shape.head.dim))
        case (2, 2, true, false) => List((x_type.shape.head.dim, y_type.shape.head.dim))
        case (2, 2, false, true) => List((x_type.shape.last.dim, y_type.shape.last.dim))
        case (2, 2, true, true) => List((x_type.shape.head.dim, y_type.shape.last.dim))
        case r => throw new Exception(s"not yet handling ranks $r")
      }
    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
      weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
      gradMap: GradMapWrapper,
      momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
      transform: Backend.Exp => Backend.Exp) = node match {

    case Node(s, "tensor_dot", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      // save forward op in forwardNodes
      forwardNodes += node
      // save backward op in backwardNodes
      (() => {
        val b_tensor = new TENSOR(transform(b))
        val a_grad = DotWithTranspose(gradMap(s), b_tensor, anno, transL = false, transR = true)
        Accumulate(gradMap(a), a_grad, anno); ()
      }) +=: backwardNodes
      (() => {
        val a_tensor = new TENSOR(transform(a))
        val b_grad = DotWithTranspose(a_tensor, gradMap(s), anno, transL = true, transR = false)
        Accumulate(gradMap(b), b_grad, anno); ()
      }) +=: backwardNodes

    case Node(s, "tensor_dot_with_transpose", _, _) => throw new Exception("implement me")

    case n => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
  }
}


trait FixedSizeDistributedTensorOpsGemm extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._

  implicit class TensorOpsGemm[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)

    def gemm(y: Rep[Tensor[T]], anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = Dot(self, tensor(y), anno)
      Wrap[Tensor[T]](t.x)
    }

    def gemmT(y: Rep[Tensor[T]], anno: Anno, transL: Boolean = false, transR: Boolean = false)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = DotWithTranspose(self, tensor(y), anno, transL, transR)
      Wrap[Tensor[T]](t.x)
    }
  }
}

