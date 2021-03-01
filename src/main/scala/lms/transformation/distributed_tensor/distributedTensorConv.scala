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

trait FixedSizeDistributedTensorConvTypeLess extends FixedSizeDistributedTensorMutationTypeLess with CudnnUtils {
  import BaseTypeLess._

  def ConvForward(input: TENSOR, filter: TENSOR, params: ConvParam, anno: Anno, __pos: SourceContext): TENSOR = {

    val ConvParam(alpha, beta, padding, strides, dilation) = params

    require(input.shapeSize.size == CUDNN_TENSOR_DIM, "input tensor of convolution must be 4D, found: " + input.shapeSize.size)
    require(filter.shapeSize.size == CUDNN_TENSOR_DIM, "input filter of convolution must be 4D, found: " + filter.shapeSize.size)
    require(padding.size == CUDNN_PARAM_DIM, "padding must be sequence of integer of length 2, found: " + padding.size)
    require(strides.size == CUDNN_PARAM_DIM, "strides must be sequence of integer of length 2, found: " + strides.size)
    require(dilation.size == CUDNN_PARAM_DIM, "dilation must be sequence of integer of length 2, found: " + dilation.size)
    require(input.et == filter.et, "input tensor element type must be equal to filter tensor element type")

    val res_tt = ConvForwardOutTensorType(input, filter, params, anno, __pos)
    (new TENSOR(Adapter.g.reflectRead("tensor_conv", C(res_tt), C(anno),
      input.x, filter.x, C(params))(input.x, filter.x))).withSrcType(__pos, input.et)
  }

  def ConvBackwardData(input: TENSOR, filter: TENSOR, doutput: TENSOR, params: ConvParam, anno: Anno, __pos: SourceContext): TENSOR = {
    assert(input.et == filter.et && filter.et == doutput.et)
    val res_tt = input.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_conv_bwd_data", C(res_tt), C(anno),
      input.x, filter.x, doutput.x, C(params))(input.x, filter.x, doutput.x)).withSrcType(__pos, input.et))
  }

  def ConvBackwardFilter(input: TENSOR, filter: TENSOR, doutput: TENSOR, params: ConvParam, anno: Anno, __pos: SourceContext): TENSOR = {
    assert(input.et == filter.et && filter.et == doutput.et)
    val res_tt = filter.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_conv_bwd_filter", C(res_tt), C(anno),
      input.x, filter.x, doutput.x, C(params))(input.x, filter.x, doutput.x)).withSrcType(__pos, input.et))
  }

  def ConvForwardOutTensorType(input: TENSOR, filter: TENSOR, params: ConvParam, anno: Anno, __pos: SourceContext): TensorType = {
    // add assertions / requires
    val input_shape = input.resultType.shape
    val filter_shape = filter.resultType.shape
    val ConvParam(alpha, beta, padding, strides, dilation) = params

    val output_N = input_shape(CUDNN_N).size
    val output_C = filter_shape(CUDNN_C_OUT).size

    def outputDim(inputDim: Int, pad: Int, filterDim: Int, dil: Int, str: Int) =
      1 + (inputDim + 2*pad - (((filterDim-1)*dil)+1)) / str

    val output_H = outputDim(input_shape(CUDNN_H).size, padding(CUDNN_PARAM_H),
      filter_shape(CUDNN_H).size, dilation(CUDNN_PARAM_H), strides(CUDNN_PARAM_H))
    val output_W = outputDim(input_shape(CUDNN_W).size, padding(CUDNN_PARAM_W),
      filter_shape(CUDNN_W).size, dilation(CUDNN_PARAM_W), strides(CUDNN_PARAM_W))

    val output_shape = Seq(
      Size(Dim(CUDNN_N), output_N),
      Size(Dim(CUDNN_C), output_C),
      Size(Dim(CUDNN_H), output_H),
      Size(Dim(CUDNN_W), output_W))

    TensorType(output_shape, input.et, anno)
  }
  
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

  def ActivationForward(input: TENSOR, params: ActivationParam, mode: String, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = input.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_activation", C(res_tt), C(anno), input.x, 
      C(params), C(mode))(input.x)).withSrcType(__pos, input.et))
  }

  def ActivationBackward(input: TENSOR, output: TENSOR, doutput: TENSOR, params: ActivationParam, mode: String, anno: Anno, __pos: SourceContext): TENSOR = {
    val res_tt = doutput.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_activation_bwd", C(res_tt), C(anno), input.x, output.x, doutput.x, 
      C(params), C(mode))(input.x, output.x, doutput.x)).withSrcType(__pos, doutput.et))
  }

  def DropoutForward(input: TENSOR, params: DropoutParam, anno: Anno, __pos: SourceContext): TENSORS = {
    val output_tt = input.resultType
    val dummy_tt = TensorType(input.resultType.shape, manifest[Boolean])  // dummy shape
    val res_tt = List(output_tt, dummy_tt)

    (new TENSORS(Adapter.g.reflectRead("tensors_dropout", C(res_tt), C(anno), 
      input.x, C(params))(input.x))).withSrcType(__pos, input.et)
  }

  def DropoutBackward(doutput: TENSOR, reserveSpace: TENSOR, params: DropoutParam, anno: Anno,  __pos: SourceContext): TENSOR = {
    val res_tt = doutput.resultType
    (new TENSOR(Adapter.g.reflectRead("tensor_dropout_bwd", C(res_tt), C(anno),
      doutput.x, reserveSpace.x, C(params))(doutput.x, reserveSpace.x)).withSrcType(__pos, doutput.et))
  }

  override def mergable_dims(node: Node) = node match {
    // constraints:
    // input.channels = filter.input_channels
    // output.channels = filter.output_channels
    case Node(s, "tensor_conv", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::Backend.Const(params:ConvParam)::_, _) =>
      val input_type    = (new TENSOR(a, useOldMetadata=true)).resultType.shape
      val filter_type   = (new TENSOR(b, useOldMetadata=true)).resultType.shape
      val output_type   = (new TENSOR(s, useOldMetadata=true)).resultType.shape
      val inputC      = input_type(CUDNN_C).dim
      val outputC     = output_type(CUDNN_C).dim
      val filterCout  = filter_type(CUDNN_C_OUT).dim
      val filterCin   = filter_type(CUDNN_C_IN).dim
      List((inputC, filterCin), (outputC, filterCout))
    
    // dropout operation has no mergable dims
    case Node(s, "tensors_dropout", _, _) => List()

    case Node(s, "tensor_softmax", _, _) => List()
    case Node(s, "tensor_activation", _, _) => List()

    case _ => super.mergable_dims(node)
  }

  override def aircopCollect(node: Node, forwardNodes: mutable.ArrayBuffer[Node],
    weightNodes: mutable.ArrayBuffer[Node], backwardNodes: mutable.ArrayBuffer[()=>Unit],
    gradMap: GradMapWrapper,
    momentumMap: mutable.HashMap[Backend.Sym, TENSOR],
    transform: Backend.Exp => Backend.Exp) = node match {
      case Node(s, "tensor_conv", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::(b:Backend.Sym)::Backend.Const(params:ConvParam)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        // save forward op in forwardNodes
        forwardNodes += node
        // save backward op in backwardNodes
        (() => {
          val x = new TENSOR(transform(a))      // weight
          val y = new TENSOR(transform(b))      // filter
          val a_grad = ConvBackwardData(x, y, gradMap(s), params, anno, pos)
          Accumulate(gradMap(a), a_grad, anno); ()
        }) +=: backwardNodes
        (() => {
          val x = new TENSOR(transform(a))      // weight
          val y = new TENSOR(transform(b))      // filter
          val b_grad = ConvBackwardFilter(x, y, gradMap(s), params, anno, pos)
          Accumulate(gradMap(b), b_grad, anno); ()
        }) +=: backwardNodes
      
      case Node(s, "tensor_softmax", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params:SoftmaxParam)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val x = new TENSOR(transform(s))
            val grad = SoftmaxBackward(x, gradMap(s), params, anno, pos)
            Accumulate(gradMap(a), grad, anno); ()
           ()
        }) +=: backwardNodes
      
      case Node(s, "tensor_activation", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params:ActivationParam)::Backend.Const(mode:String)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        forwardNodes += node
        (() => {
            val x = new TENSOR(transform(a))
            val y = new TENSOR(transform(s))
            val grad = ActivationBackward(x, y, gradMap(s), params, mode, anno, pos)
            Accumulate(gradMap(a), grad, anno); ()
           ()
        }) +=: backwardNodes
    
      case Node(s, "tensors_dropout", tt::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params:DropoutParam)::_, _) =>
        implicit val pos = Adapter.oldSourceMap(s)
        // save forward op in forwardNodes
        forwardNodes += node
        // save backward op in backwardNodes

        (() => {
          val x = new TENSORS(transform(s))
          val grads = gradMap.getGradsOfOp(s)
          val g = DropoutBackward(grads(0), TENSORS.getResult(x, 1), params, anno, pos)
          Accumulate(gradMap(a), g, anno); ()
        }) +=: backwardNodes

      case _ => super.aircopCollect(node, forwardNodes, weightNodes, backwardNodes, gradMap, momentumMap, transform)
    }
}

trait FixedSizeDistributedTensorOpsConv extends FixedSizeDistributedTensorOpsBase {
  import FixedSizeDistributedTensorTypeLess._
  import scala.collection.immutable.Seq

  implicit class TensorOpsConv[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    val self = tensor(x)
    val conv_params_def = ConvParam(1.0f, 0.0f, Seq(1, 1), Seq(1, 1), Seq(1, 1))  // default convolution parameter settings
    val dropout_params_def = DropoutParam(0.5f, 1)

    def conv(y: Rep[Tensor[T]], anno: Anno, params: ConvParam = conv_params_def)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = ConvForward(self, tensor(y), params, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    val softmax_params_def = SoftmaxParam(1.0f, 0.0f)
    def softmax(anno: Anno, params: SoftmaxParam = softmax_params_def)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = SoftmaxForward(self, params, anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def sigmoid(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, 0.0f)
      val t = ActivationForward(self, p, "sigmoid", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }
    
    def tanh(anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, 0.0f)
      val t = ActivationForward(self, p, "tanh", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    // clipped relu
    def relu(threshold: Float, anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, threshold)
      val t = ActivationForward(self, p, "crelu", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }

    def elu(alpha: Float, anno: Anno)(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val self = tensor(x)
      val p = ActivationParam(1.0f, 0.0f, alpha)
      val t = ActivationForward(self, p, "elu", anno, __pos)
      Wrap[Tensor[T]](t.x)
    }
    
    def dropout(anno: Anno, params: DropoutParam = dropout_params_def)(implicit __pos: SourceContext): List[Rep[Tensor[T]]] = {
      val op = DropoutForward(self, params, anno, __pos)
      ((0 until 1): Range).toList.map(i => Wrap[Tensor[T]](TENSORS.getResult(op, i).x))
    }
  }
}
