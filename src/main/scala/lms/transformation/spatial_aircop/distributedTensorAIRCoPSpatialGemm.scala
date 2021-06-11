package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialGemm extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_dot", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)

      // then we should run this dot op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          Dot(left_operand, right_operand).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val dot = Dot(left_operand, right_operand)
          AllReduce(dot).x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_dot")
      }

    case Node(s, "tensor_dot_with_transpose", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(transL:Boolean)::Backend.Const(transR:Boolean)::(left:Backend.Sym)::(right:Backend.Sym)::_,_) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)

      // then we should run this dot op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          DotWithTranspose(left_operand, right_operand, NAnno, transL, transR).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val dot = DotWithTranspose(left_operand, right_operand, NAnno, transL, transR)
          AllReduce(dot).x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_dot_with_transpose")
      }

    case _ => super.transform(n)
  }
}
