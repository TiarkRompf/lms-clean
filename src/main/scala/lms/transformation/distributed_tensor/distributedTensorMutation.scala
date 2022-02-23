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
  import PrimitiveTypeLess._
  import ArrayTypeLess._

  class TENSORARRAY(override val x: Backend.Exp, override val useOldMetadata: Boolean = false) extends TOP(x, useOldMetadata) {
    def withEleType(m: Manifest[_]): this.type = { Adapter.typeMap(x) = m; this }
    override def withSrcType(pos: SourceContext, m: Manifest[_]): this.type =
      withSource(pos).withEleType(m)
    val (resultType: TensorType, annotation: Anno, length:Int) = gc.get(x.asInstanceOf[Backend.Sym]) match {
      case Some(Node(_, s, Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::Backend.Const(l:Int)::_, _))
        if s.startsWith("tensorarray_") => {
          if (l < 1) throw new Exception(s"Tensor Array $x has length less than 1")
          (tt, anno, l)
        }
      case a => throw new Exception(s"Node $a is not an Tensor Array node")
    }

    def pos: SourceContext = if (useOldMetadata) Adapter.oldSourceMap(x) else Adapter.sourceMap(x)

    def et: Manifest[_] = if (useOldMetadata) Adapter.oldTypeMap(x) else Adapter.typeMap(x)

    val shapeSize: Seq[Int] = resultType.shape.map(_.size)
  }

  object TENSORARRAY {
    def get(x: TENSORARRAY, i: INT)(implicit __pos: SourceContext) = {
      // get tensor array
      (new TENSOR(Adapter.g.reflectRead("tensor_array_get", C(x.resultType), C(x.annotation), x.x, i.x)(x.x, i.x))).withSrcType(__pos, x.resultType.et)
    }

    def set(x: TENSORARRAY, i: INT, y: TENSOR)(implicit __pos: SourceContext):UNIT = {
      // set tensor array
      assert(x.annotation == y.annotation)
      assert(x.resultType == y.resultType)
      //assert(x.resultType.shapeSize == y.resultType.shapeSize)
      UNIT(Adapter.g.reflectEffect("tensor_array_set", C(x.resultType), C(x.annotation), x.x, i.x, y.x)(x.x, i.x, y.x)(x.x))
    }

    def isTensorArray(x: Backend.Exp, useOldMetadata: Boolean = false) = {
      val gc = if (useOldMetadata) Adapter.oldDefsCache else Adapter.g.globalDefsCache
      gc.get(x.asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, s, _, _)) => s.startsWith("tensorarray_")
        case a => false
      }
    }
  }

  def ZEROSARRAY(tt: TensorType, anno: Anno, length:Int)(implicit __pos: SourceContext): TENSORARRAY = {
    (new TENSORARRAY(Adapter.g.reflectMutable("tensorarray_zeros", C(tt), C(anno), C(length)))).withSrcType(__pos, tt.et)
  }

  def Accumulate(x: TENSOR, y: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): UNIT = {
    UNIT(Adapter.g.reflectEffect("accum_tensor", C(anno), x.x, y.x)(x.x, y.x)(x.x))
  }

  def Optimize(x: TENSOR, grad: TENSOR, momentum: TENSOR, anno: Anno = NAnno)(implicit __pos: SourceContext): UNIT = {
    UNIT(Adapter.g.reflectEffect("optimize_tensor", C(anno), x.x, grad.x, momentum.x)(x.x, grad.x, momentum.x)(x.x, Adapter.CTRL))
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
      s"$s = accum_tensor($x, $y) (${symTensorShape(x, graph)}, ${symTensorShape(y, graph)})${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "optimize_tensor", Backend.Const(anno:Anno)::(x:Backend.Sym)::(g:Backend.Sym)::(m:Backend.Sym)::_, _) =>
      s"$s = optimize_tensor($x, $g, $m) (${symTensorShape(x, graph)}, ${symTensorShape(g, graph)}, ${symTensorShape(m, graph)})${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "all_reduce_tensor", Backend.Const(devices:Seq[Device])::(x:Backend.Sym)::Backend.Const(mode:String)::_, _) =>
      s"$s = all_reduce_tensor($x, mode=$mode, devices=$devices) (${symTensorShape(x, graph)})"
    case Node(s, "tensorarray_zeros", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(length)::_, _) =>
      s"$s = tensorarray_zeros() -> length=${length}, type = ${tt.toString}${if (anno != NAnno) s"  Anno: $anno" else ""}"
    case Node(s, "tensor_array_get", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::(i:Backend.Sym)::_, _) =>
      s"$s = tensor_array_get(array=$x, index=$i, type=${tt.toString})"
    case Node(s, "tensor_array_set", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::(i:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      s"$s = tensor_array_set(array=$x, index=$i, value=$y, type=${tt.toString})"
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
