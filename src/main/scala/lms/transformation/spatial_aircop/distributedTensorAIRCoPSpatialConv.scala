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
    case Node(s, "tensor_conv", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in tensor_conv")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ConvForward(left_operand, right_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.ConvParam], NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"todo")
        case a => throw new Exception(s"not yet handling annotation $a")
      }

    case Node(s, "tensor_conv_bwd_data", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::
      (input:Backend.Sym)::(filter:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val input_operand = get_operand(input, anno)
      val filter_operand = get_operand(filter, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ConvBackwardData(input_operand, filter_operand, doutput_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.ConvParam], NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception("todo")
        case a => throw new Exception(s"not yet handling annotation $a")
      }

    case Node(s, "tensor_conv_bwd_filter", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::
      (input:Backend.Sym)::(filter:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val input_operand = get_operand(input, anno)
      val filter_operand = get_operand(filter, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val dfilter = ConvBackwardFilter(input_operand, filter_operand, doutput_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.ConvParam], NAnno, pos)
          AllReduce(dfilter).x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_conv")
      }

    case Node(s, "tensors_dropout", Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val input_operand = get_operand(input, anno)

      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tts(0).contains(dim) =>
          DropoutForward(input_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.DropoutParam], NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception("todo")
        case a => throw new Exception(s"not yet handling annotation $a")
      }

    case Node(s, "tensor_dropout_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(doutput:Backend.Sym)::
      (reserveSpace:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val doutput_operand = get_operand(doutput, anno)
      val reserveSpace_operand = get_operand(reserveSpace, anno)

      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          DropoutBackward(doutput_operand, reserveSpace_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.DropoutParam], NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception("todo")
        case a => throw new Exception(s"not yet handling annotation $a")
      }

    case Node(s, "tensor_pooling", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      Backend.Const(params)::Backend.Const(mode:String)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val input_operand = get_operand(input, anno)
      System.out.println("test")
      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          PoolingForward(input_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.PoolingParam], mode, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception("todo")
        case a => throw new Exception(s"not yet handling annotation $a")
      }

    case Node(s, "tensor_pooling_bwd",Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::(output:Backend.Sym)::
      (doutput:Backend.Sym)::Backend.Const(params)::Backend.Const(mode:String)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val input_operand = get_operand(input, anno)
      val output_operand = get_operand(output, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception("todo")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          PoolingBackward(input_operand, output_operand, doutput_operand, params.asInstanceOf[FixedSizeDistributedTensorTypeLess.PoolingParam], mode, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception("todo")
        case a => throw new Exception(s"not yet handling annotation $a")
      }

    case Node(s, "tensor_softmax", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val operand = get_operand(a, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op tensor_softmax")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          SoftmaxForward(operand, params.asInstanceOf[SoftmaxParam], NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op tensor_softmax")
      }

    case Node(s, "tensor_softmax_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(output:Backend.Sym)::(doutput:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val output_operand = get_operand(output, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op tensor_softmax_bwd")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          SoftmaxBackward(output_operand, doutput_operand, params.asInstanceOf[SoftmaxParam], NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op tensor_softmax_bwd")
      }

    case Node(s, "tensor_activation", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params)::
      Backend.Const(mode:String)::_, _) =>

      implicit val pos = Adapter.oldSourceMap(s)

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val operand = get_operand(a, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op tensor_activation")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ActivationForward(operand, params.asInstanceOf[ActivationParam], mode, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op tensor_activation")
      }

    case Node(s, "tensor_activation_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      (output:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::Backend.Const(mode:String)::_, _) =>

      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      // load the `left` and `right`, maybe add communication ops to resolve split annotation conflicts
      val input_operand = get_operand(input, anno)
      val output_operand = get_operand(output, anno)
      val doutput_operand = get_operand(doutput, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in op tensor_activation_bwd")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          ActivationBackward(input_operand, output_operand, doutput_operand, params.asInstanceOf[ActivationParam], mode, NAnno, pos).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in op tensor_activation_bwd")
      }

    case _ => super.transform(n)
  }
}
