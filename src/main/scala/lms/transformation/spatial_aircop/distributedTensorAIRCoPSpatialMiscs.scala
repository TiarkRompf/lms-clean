package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.ListBuffer

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialMiscs extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_maskedfill", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::(mask:Backend.Sym)::
      Backend.Const(value:Float)::_, _) => // FIXME(feiw): value is hard-coded to be Float
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      val input_operand = get_operand(input, anno)
      val mask_operand = get_operand(mask, anno)
      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          MaskedFillForward(input_operand, mask_operand, value, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }

    case Node(s, "tensor_maskedfill_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(doutput:Backend.Sym)::(mask:Backend.Sym)::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      val doutput_operand = get_operand(doutput, anno)
      val mask_operand = get_operand(mask, anno)
      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          MaskedFillBackward(doutput_operand, mask_operand, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }

    case Node(s, "tensor_logsoftmax", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      val input_operand = get_operand(input, anno)
      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          LogSoftmaxForward(input_operand, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }

    case Node(s, "tensor_logsoftmax_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(output:Backend.Sym)::(doutput:Backend.Sym)::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      val output_operand = get_operand(output, anno)
      val doutput_operand = get_operand(doutput, anno)
      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          LogSoftmaxBackward(output_operand, doutput_operand, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }

    case Node(s, "tensor_transpose", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(operand:Backend.Sym)::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      val input = get_operand(operand, anno)
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) => Transpose(input).x
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }
    
    case Node(s, "tensor_permute", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(operand:Backend.Sym)::Backend.Const(perm:List[Int])::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      val input = get_operand(operand, anno)
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) => Permute(input, perm, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) => throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled")
      }

    case _ => super.transform(n)
  }
}
