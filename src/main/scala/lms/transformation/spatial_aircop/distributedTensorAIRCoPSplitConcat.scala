package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSplitConcat extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensors_split", Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::(input:Backend.Sym)::Backend.Const(axis:Int)::_, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)

      val operand = get_operand(input, anno) // input tensor

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in mult op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tts(0).contains(dim) && dim != axis =>
          Split(operand, axis, tts.map(_.shapeSize(axis))).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce in split")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
      }

    case Node(s, "tensor_concat", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::Backend.Const(axis:Int)::(inputs:List[Backend.Sym]), _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)

      val input_operands = inputs.map(get_operand(_, anno))

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in concat op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          Concat(input_operands, axis).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce in concat")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_concat")
      }

    case _ => super.transform(n)
  }
}
