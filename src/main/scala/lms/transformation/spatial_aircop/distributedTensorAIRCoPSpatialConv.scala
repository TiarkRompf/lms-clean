package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialConv extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._


  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, op@"tensor_conv", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::
      Backend.Const(params)::_, _) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ConvForward(left_operand, right_operand, params.asInstanceOf[ConvParam], anno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op ${op}")
      }

    case Node(s, op, Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::
      (weight:Backend.Sym)::(filter:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::_, _) 
      if op == "tensor_conv_bwd_data" || op == "tensor_conv_bwd_filter" =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val weight_operand = get_operand(weight, anno)
      val filter_operand = get_operand(filter, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          if (op == "tensor_conv_bwd_data")
            ConvBackwardData(weight_operand, filter_operand, doutput_operand, params.asInstanceOf[ConvParam], anno, pos).x
          else
            ConvBackwardFilter(weight_operand, filter_operand, doutput_operand, params.asInstanceOf[ConvParam], anno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op ${op}")
      }

    case Node(s, op@"tensor_softmax", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val operand = get_operand(a, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          SoftmaxForward(operand, params.asInstanceOf[SoftmaxParam], anno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op ${op}")
      }

    case Node(s, op@"tensor_softmax_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(output:Backend.Sym)::(doutput:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val output_operand = get_operand(output, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          SoftmaxBackward(output_operand, doutput_operand, params.asInstanceOf[SoftmaxParam], anno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op ${op}")
      }
    
    case Node(s, op@"tensor_activation", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params)::
      Backend.Const(mode:String)::_, _) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val operand = get_operand(a, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ActivationForward(operand, params.asInstanceOf[ActivationParam], mode, anno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op ${op}")
      }
    
    case Node(s, op@"tensor_activation_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      (output:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::Backend.Const(mode:String)::_, _) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val input_operand = get_operand(input, anno)
      val output_operand = get_operand(output, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op ${op}")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ActivationBackward(input_operand, output_operand, doutput_operand, params.asInstanceOf[ActivationParam], mode, anno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op ${op}")
      }


    case _ => super.transform(n)
  }
}