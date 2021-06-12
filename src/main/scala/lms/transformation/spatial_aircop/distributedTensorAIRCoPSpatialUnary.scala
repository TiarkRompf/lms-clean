package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialUnary extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  val unaryOps = List("tensor_negate", "tensor_invert", "tensor_tanh", "tensor_relu", "tensor_transpose")

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, op, Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(operand:Backend.Sym)::_, _)
      if unaryOps.contains(op) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val input = get_operand(operand, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          // FIXME(feiw): how do we know that all devices in the rank wants the op?
          op match {
            case "tensor_negate" => Negate(input).x
            case "tensor_invert" => Invert(input).x
            case "tensor_relu" => Relu(input).x
            case "tensor_tanh" => Tanh(input).x
            case "tensor_transpose" => Transpose(input).x
            case _ => throw new Exception(s"op $op is a binary op that has not been handled")
          }
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in $op")
      }
    case _ => super.transform(n)
  }
}
