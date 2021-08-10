package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialBinary extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  val binaryOps = List("tensor_add", "tensor_sub", "tensor_mul", "tensor_div", "tensor_tanh_grad", "tensor_relu_grad", "tensor_invert_grad")

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, op, Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _)
      if binaryOps.contains(op) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          // FIXME(feiw): how do we know that all devices in the rank wants the op?
          op match {
            case "tensor_add" => Add(left_operand, right_operand).x
            case "tensor_sub" => Sub(left_operand, right_operand).x
            case "tensor_mul" => Mul(left_operand, right_operand).x
            case "tensor_div" => Div(left_operand, right_operand).x
            case "tensor_tanh_grad" => TanhGrad(left_operand, right_operand).x
            case "tensor_relu_grad" => ReluGrad(left_operand, right_operand).x
            case "tensor_invert_grad" => InvertGrad(left_operand, right_operand).x
            case _ => throw new Exception(s"op $op is a binary op that has not been handled")
          }
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
      }
    case _ => super.transform(n)
  }
}
