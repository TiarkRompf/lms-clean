package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.ListBuffer

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps, CUDNNTypeLess, CLibTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.{DataStructure, CudnnUtils}

import Backend._


trait DistributeTensor2MPI_NCCLConv extends DistributeTensor2MPI_NCCLBase with CudnnUtils {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import RangeTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  import CUDATypeLess._
  import RandomDataTypeLess._
  import NCCLTypeLess._
  import SIZE_TTypeLess._
  import CUBLASTypeLess._
  import CUDNNTypeLess._
  import CLibTypeLess._


  // given the shape of a weight tensor or filter(kernel) tensor, return its descriptor.
  // if cudnnTensor2Desc contains a descriptor matching the required shape, return that descriptor
  // otherwise, create a new descriptor, store it in the hashmap and return it.
  def getTensorDescriptor(shape: Seq[Int], kind: String)(implicit __pos: SourceContext): TOP = cudnnTensor2Desc.get(shape) match {
    case Some((desc, _)) => desc
    case None => kind match {
      case "tensor" =>
        generate_comment(s"begin creating and setting tensor descriptor of shape ${shape}")
        val desc = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_TENSOR_DESCRIPTOR], "cudnnTensorDescriptor_t").x)
        CUDNN_CHECK(CUDNN_CREATE_TENSOR_DESCRIPTOR(desc))
        CUDNN_CHECK(CUDNN_SET_TENSOR_4D_DESCRIPTOR(desc, CUDNN_NCHW, CUDNN_FLOAT,
          INT(shape(CUDNN_N)), INT(shape(CUDNN_C)), INT(shape(CUDNN_H)), INT(shape(CUDNN_W))))
        generate_comment(s"end creating and setting tensor descriptor")
        cudnnTensor2Desc += ((shape, (desc, kind)))
        desc
      case "filter" =>
        generate_comment(s"begin creating and setting filter descriptor of shape ${shape}")
        val desc = new CUDNN_FILTER_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_FILTER_DESCRIPTOR], "cudnnFilterDescriptor_t").x)
        CUDNN_CHECK(CUDNN_CREATE_FILTER_DESCRIPTOR(desc))
        CUDNN_CHECK(CUDNN_SET_FILTER_4D_DESCRIPTOR(desc, CUDNN_FLOAT, CUDNN_NCHW,
          INT(shape(CUDNN_C_OUT)), INT(shape(CUDNN_C_IN)), INT(shape(CUDNN_H)), INT(shape(CUDNN_W))))
        generate_comment(s"end creating and setting filter descriptor")
        cudnnTensor2Desc += ((shape, (desc, kind)))
        desc
      case _ => throw new Exception("Unknown kind of cudnn tensor descriptor")
    }
  }

  // given the padding, strides and dilation parameters of a convolution operation, return its descriptor.
  // if cudnnTensor2Desc contains a descriptor matching the required parameters, return that descriptor
  // otherwise, create a new descriptor, store it in the hashmap and return it.
  def getConvDescriptor(padding: Seq[Int], strides: Seq[Int], dilation: Seq[Int])(implicit __pos: SourceContext): CUDNN_CONV_DESCRIPTOR = {
    val key = padding ++ strides ++ dilation
    cudnnConv2Desc.get(key) match {
      case Some(desc) => desc
      case None =>
        generate_comment(s"begin creating and setting convolution descriptor of padding: ${padding}, strides: ${strides}, dilation: ${dilation}")
        val desc = new CUDNN_CONV_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
        CUDNN_CHECK(CUDNN_CREATE_CONV_DESCRIPTOR(desc))
        CUDNN_CHECK(CUDNN_SET_CONV_2D_DESCRIPTOR(desc,
          INT(padding(CUDNN_PARAM_H)),  INT(padding(CUDNN_PARAM_W)),
          INT(strides(CUDNN_PARAM_H)),  INT(strides(CUDNN_PARAM_W)),
          INT(dilation(CUDNN_PARAM_H)), INT(dilation(CUDNN_PARAM_W)),
          CUDNN_CROSS_CORRELATION, CUDNN_FLOAT))
        generate_comment(s"end creating and setting convolution descriptor")
        cudnnConv2Desc += ((key, desc))
        desc
    }
  }

  def getActivationDescriptor(mode: String, coef: Float)(implicit __pos: SourceContext): CUDNN_ACTIVATION_DESCRIPTOR = {
    val key = (mode, coef)
    cudnnActv2Desc.get(key) match {
      case Some(desc) => desc
      case None =>
        generate_comment(s"begin creating and setting activation descriptor")
        val desc = new CUDNN_ACTIVATION_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_ACTIVATION_DESCRIPTOR], "cudnnActivationDescriptor_t").x)
        CUDNN_CHECK(CUDNN_CREATE_ACTIVATION_DESCRIPTOR(desc))
        val cudnnMode: TOP = mode match {
          case "relu"       => CUDNN_ACTIVATION_RELU
          case "crelu"      => CUDNN_ACTIVATION_CLIPPED_RELU
          case "sigmoid"    => CUDNN_ACTIVATION_SIGMOID
          case "tanh"       => CUDNN_ACTIVATION_TANH
          case "elu"        => CUDNN_ACTIVATION_ELU
          case "identity"   => CUDNN_ACTIVATION_IDENTITY
          case _            => CUDNN_ACTIVATION_RELU  // default
        }
        CUDNN_CHECK(CUDNN_SET_ACTIVATION_DESCRIPTOR(desc, cudnnMode, CUDNN_PROPAGATE_NAN, FLOAT(coef)))
        generate_comment(s"end creating and setting activation descriptor")
        cudnnActv2Desc += ((key, desc))
        desc
    }
  }

  def getPoolingDescriptor(mode: String, window: Seq[Int], padding: Seq[Int], strides: Seq[Int])(implicit __pos: SourceContext): CUDNN_POOLING_DESCRIPTOR = {
    val key = (mode, window ++ padding ++ strides)
    cudnnPool2Desc.get(key) match {
      case Some(desc) => desc
      case None =>
        generate_comment(s"begin creating and setting pooling descriptor")
        val desc = new CUDNN_POOLING_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_POOLING_DESCRIPTOR], "cudnnPoolingDescriptor_t").x)
        CUDNN_CHECK(CUDNN_CREATE_POOLING_DESCRIPTOR(desc))
        val cudnnMode: TOP = mode match {
          case "max"          => CUDNN_POOLING_MAX
          case "avg_in_pad"   => CUDNN_POOLING_AVERAGE_COUNT_INCLUDE_PADDING
          case "avg_ex_pad"   => CUDNN_POOLING_AVERAGE_COUNT_EXCLUDE_PADDING
          case "max_dtm"      => CUDNN_POOLING_MAX_DETERMINISTIC
          case _              => CUDNN_POOLING_MAX
        }
        CUDNN_CHECK(CUDNN_SET_POOLING_2D_DESCRIPTOR(desc, cudnnMode, CUDNN_PROPAGATE_NAN,
          INT(window(CUDNN_PARAM_H)),   INT(window(CUDNN_PARAM_W)),
          INT(padding(CUDNN_PARAM_H)),  INT(padding(CUDNN_PARAM_W)),
          INT(strides(CUDNN_PARAM_H)),  INT(strides(CUDNN_PARAM_W))))
        generate_comment(s"end creating and setting pooling descriptor")
        cudnnPool2Desc += ((key, desc))
        desc
    }
  }

  def getDropoutDescriptor(states: ARRAY, states_bytes: VAR, params: DropoutParam)(implicit __pos: SourceContext): CUDNN_DROPOUT_DESCRIPTOR = {
    val DropoutParam(dropout, seed) = params
    generate_comment("begin creating dropout descriptor")
    val desc = new CUDNN_DROPOUT_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_DROPOUT_DESCRIPTOR], "cudnnDropoutDescriptor_t").x)
    CUDNN_CHECK(CUDNN_CREATE_DROPOUT_DESCRIPTOR(desc))
    CUDNN_CHECK(CUDNN_SET_DROPOUT_DESCRIPTOR(desc, myCUDNNComm, dropout, states, states_bytes, seed))
    generate_comment("end creating dropout descriptor")
    desc
  }


  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_conv", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack convolution paratemers
      val ConvParam(alpha, beta, padding, strides, dilation) = params.asInstanceOf[ConvParam]

      // get input info and transform input tensors
      val weight_shape = tensor_shape(left, useOldMetadata = true)
      val filter_shape = tensor_shape(right, useOldMetadata = true)
      val weight_tensor = get_operand(left, anno)
      val filter_tensor = get_operand(right, anno)

      val input_descriptor = getTensorDescriptor(weight_shape, "tensor")
      val filter_descriptor = getTensorDescriptor(filter_shape, "filter")
      val conv_descriptor = getConvDescriptor(padding, strides, dilation)

      val output_shape = tensor_shape(s, useOldMetadata = true)
      val output_descriptor = getTensorDescriptor(output_shape, "tensor")

      val output_size = numeral(output_shape)
      val output = withComment(s"allocating gpu array of size $output_size and type Float for the output of convolution") {
        gpu_array(output_size, manifest[Float], myCUDADevice)
      }

      val convAlgo = withComment("finding convolution forward algorithm") {
        var res_count = 0
        val res = new CUDNN_CONV_FWD_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_FWD_ALG_PERF], "cudnnConvolutionFwdAlgoPerf_t").x)
        CUDNN_CHECK(CUDNN_FIND_CONV_FWD_ALG(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor, INT(1), VAR(res_count), res))
        val convAlgoRep = readField[Manifest[CUDNN_CONV_FWD_ALG_PERF], Manifest[CUDNN_CONV_FWD_ALGO]](Wrap[Manifest[CUDNN_CONV_FWD_ALG_PERF]](res.x), "algo")
        TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_FWD_ALGO])
      }

      generate_comment("begin finding convolution forward workspace size")
      var workspace_bytes = 0
      val workspace_bytes_v = VAR(SIZE_T(workspace_bytes)) // var read
      CUDNN_CHECK(CUDNN_GET_CONV_FWD_WORKSPACE_SZ(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor,
        convAlgo, workspace_bytes_v))
      generate_comment("begin finding convolution backward workspace size")

      val d_workspace = withComment("allocating gpu array for convolution forward workspace") {
        GPU_ARRAY_BY_BYTE(INT(workspace_bytes_v(pos)), manifest[Float], myCUDADevice)
      }

      withComment("convolution forward pass") {
        CUDNN_CHECK(CUDNN_CONV_FWD(myCUDNNComm, VAR(FLOAT(alpha)), input_descriptor, new ARRAY(weight_tensor), filter_descriptor, new ARRAY(filter_tensor),
        conv_descriptor, convAlgo, d_workspace, workspace_bytes_v, VAR(FLOAT(beta)), output_descriptor, output))
      }
      output.x

    case Node(s, "tensor_conv_bwd_data", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::
      (weight:Backend.Sym)::(filter:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack convolution paratemers
      val ConvParam(alpha, beta, padding, strides, dilation) = params.asInstanceOf[ConvParam]

      val doutput_shape = tensor_shape(doutput, useOldMetadata = true)
      val weight_shape = tensor_shape(weight, useOldMetadata = true)
      val filter_shape = tensor_shape(filter, useOldMetadata = true)

      val filter_tensor = get_operand(filter, anno)
      val output_tensor = get_operand(doutput, anno)

      val filter_descriptor = getTensorDescriptor(filter_shape, "filter")
      val doutput_descriptor = getTensorDescriptor(doutput_shape, "tensor")
      val dweight_descriptor = getTensorDescriptor(weight_shape, "tensor")  // should this descriptor just be weight descriptor?
      val conv_descriptor = getConvDescriptor(padding, strides, dilation)

      val dweight_size = numeral(weight_shape)
      val dweight = withComment(s"allocating gpu array of size $dweight_size and type Float for the gradient weight of convolution") {
        gpu_array(dweight_size, manifest[Float], myCUDADevice)
      }

      val convAlgo = withComment("finding convolution backward data algorithm") {
        var res_count = 0
        val res = new CUDNN_CONV_BWD_DATA_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_BWD_DATA_ALG_PERF], "cudnnConvolutionBwdDataAlgoPerf_t").x)
        CUDNN_CHECK(CUDNN_FIND_CONV_BWD_DATA_ALG(myCUDNNComm, filter_descriptor, doutput_descriptor, conv_descriptor, dweight_descriptor, INT(1), VAR(res_count), res))
        val convAlgoRep = readField[Manifest[CUDNN_CONV_BWD_DATA_ALG_PERF], Manifest[CUDNN_CONV_BWD_DATA_ALGO]](Wrap[Manifest[CUDNN_CONV_BWD_DATA_ALG_PERF]](res.x), "algo")
        TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_BWD_DATA_ALGO])
      }

      generate_comment("begin finding convolution backward data workspace size")
      var workspace_bytes = 0
      val workspace_bytes_v = VAR(SIZE_T(workspace_bytes)) // var read
      CUDNN_CHECK(CUDNN_GET_CONV_BWD_DATA_WORKSPACE_SZ(myCUDNNComm, filter_descriptor, doutput_descriptor, conv_descriptor, dweight_descriptor,
        convAlgo, workspace_bytes_v))
      generate_comment("end finding convolution backward data workspace size")

      val d_workspace = withComment("allocating gpu array for convolution backward data workspace") {
        GPU_ARRAY_BY_BYTE(INT(workspace_bytes_v(pos)), manifest[Float], myCUDADevice)
      }

      withComment("convolution backward data pass") {
        CUDNN_CHECK(CUDNN_CONV_BWD_DATA(myCUDNNComm, VAR(FLOAT(alpha)), filter_descriptor, new ARRAY(filter_tensor), doutput_descriptor, new ARRAY(output_tensor),
          conv_descriptor, convAlgo, d_workspace, workspace_bytes_v, VAR(FLOAT(beta)), dweight_descriptor, dweight))
      }
      dweight.x

    case Node(s, "tensor_conv_bwd_filter", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::
      (weight:Backend.Sym)::(filter:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack convolution paratemers
      val ConvParam(alpha, beta, padding, strides, dilation) = params.asInstanceOf[ConvParam]

      val doutput_shape = tensor_shape(doutput, useOldMetadata = true)
      val weight_shape = tensor_shape(weight, useOldMetadata = true)
      val filter_shape = tensor_shape(filter, useOldMetadata = true)

      val weight_tensor = get_operand(weight, anno)
      val doutput_tensor = get_operand(doutput, anno)

      val weight_descriptor = getTensorDescriptor(weight_shape, "tensor")
      val doutput_descriptor = getTensorDescriptor(doutput_shape, "tensor")
      val dfilter_descriptor = getTensorDescriptor(filter_shape, "filter")
      val conv_descriptor = getConvDescriptor(padding, strides, dilation)

      val dfilter_size = numeral(filter_shape)
      val dfilter = withComment(s"allocating gpu array of size $dfilter_size and type Float for the gradient filter of convolution") {
        gpu_array(dfilter_size, manifest[Float], myCUDADevice)
      }

      val convAlgo = withComment("finding convolution backward filter algorithm") {
        var res_count = 0
        val res = new CUDNN_CONV_BWD_FILTER_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_BWD_FILTER_ALG_PERF], "cudnnConvolutionBwdFilterAlgoPerf_t").x)
        CUDNN_CHECK(CUDNN_FIND_CONV_BWD_FILTER_ALG(myCUDNNComm, weight_descriptor, doutput_descriptor, conv_descriptor, dfilter_descriptor, INT(1), VAR(INT(res_count)), res))
        val convAlgoRep = readField[Manifest[CUDNN_CONV_BWD_FILTER_ALG_PERF], Manifest[CUDNN_CONV_BWD_FILTER_ALGO]](Wrap[Manifest[CUDNN_CONV_BWD_FILTER_ALG_PERF]](res.x), "algo")
        TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_BWD_FILTER_ALGO])
      }

      generate_comment("begin finding convolution backward filter workspace size")
      var workspace_bytes = 0
      val workspace_bytes_v = VAR(SIZE_T(workspace_bytes)) // var read
      CUDNN_CHECK(CUDNN_GET_CONV_BWD_FILTER_WORKSPACE_SZ(myCUDNNComm, weight_descriptor, doutput_descriptor, conv_descriptor, dfilter_descriptor,
        convAlgo, workspace_bytes_v))
      generate_comment("end finding convolution backward filter workspace size")

      val d_workspace = withComment("allocating gpu array for convolution backward filter workspace") {
        GPU_ARRAY_BY_BYTE(INT(workspace_bytes_v(pos)), manifest[Float], myCUDADevice)
      }

      withComment("convolution backward filter pass") {
        CUDNN_CHECK(CUDNN_CONV_BWD_FILTER(myCUDNNComm, VAR(FLOAT(alpha)), weight_descriptor, new ARRAY(weight_tensor), doutput_descriptor, new ARRAY(doutput_tensor),
          conv_descriptor, convAlgo, d_workspace, workspace_bytes_v, VAR(FLOAT(beta)), dfilter_descriptor, dfilter))
      }
      dfilter.x

    case Node(s, "tensor_softmax", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params)::_, _) =>

      implicit val pos = Adapter.oldSourceMap(s)

      // unpack softmax paratemers
      val SoftmaxParam(alpha, beta) = params.asInstanceOf[SoftmaxParam]

      val input_shape = tensor_shape(a, useOldMetadata = true)
      val input_tensor = get_operand(a, anno)
      val input_descriptor = getTensorDescriptor(input_shape, "tensor")

      val size = numeral(input_shape)
      val output = withComment(s"allocating gpu array of size $size and type Float for the output of softmax") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("softmax forward pass") {
        CUDNN_CHECK(CUDNN_SOFTMAX_FWD(myCUDNNComm, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, VAR(FLOAT(alpha)), input_descriptor, new ARRAY(input_tensor),
          VAR(FLOAT(beta)), input_descriptor, output))
      }
      output.x

    case Node(s, "tensor_softmax_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(output:Backend.Sym)::(doutput:Backend.Sym)::
      Backend.Const(params)::_, _) =>

      implicit val pos = Adapter.oldSourceMap(s)

      // unpack softmax paratemers
      val SoftmaxParam(alpha, beta) = params.asInstanceOf[SoftmaxParam]

      val output_shape = tensor_shape(output, useOldMetadata = true)
      val output_tensor = get_operand(output, anno)
      val doutput_tensor = get_operand(doutput, anno)

      val output_descriptor = getTensorDescriptor(output_shape, "tensor")

      val size = numeral(output_shape)
      val dinput = withComment(s"allocating gpu array of size $size and type Float for the gradient input of softmax") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("softmax backward pass") {
        CUDNN_CHECK(CUDNN_SOFTMAX_BWD(myCUDNNComm, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, VAR(FLOAT(alpha)), output_descriptor, new ARRAY(output_tensor),
          output_descriptor, new ARRAY(doutput_tensor), VAR(FLOAT(beta)), output_descriptor, dinput))
      }
      dinput.x

    case Node(s, "tensor_activation", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(a:Backend.Sym)::Backend.Const(params)::
      Backend.Const(mode:String)::_, _) =>

      implicit val pos = Adapter.oldSourceMap(s)

      // unpack softmax paratemers
      val ActivationParam(alpha, beta, coef) = params.asInstanceOf[ActivationParam]

      val input_shape = tensor_shape(a, useOldMetadata = true)
      val input_tensor = get_operand(a, anno)

      val input_descriptor = getTensorDescriptor(input_shape, "tensor")
      val activation_descriptor = getActivationDescriptor(mode, coef)

      val size = numeral(input_shape)
      val output = withComment(s"allocating gpu array of size $size and type Float for the output of softmax") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("activation forward pass") {
        CUDNN_CHECK(CUDNN_ACTIVATION_FWD(myCUDNNComm, activation_descriptor, VAR(FLOAT(alpha)), input_descriptor, new ARRAY(input_tensor),
          VAR(FLOAT(beta)), input_descriptor, output))
      }
      output.x

    case Node(s, "tensor_activation_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      (output:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params)::Backend.Const(mode:String)::_, _) =>

      implicit val pos = Adapter.oldSourceMap(s)

      // unpack softmax paratemers
      val ActivationParam(alpha, beta, coef) = params.asInstanceOf[ActivationParam]

      val input_shape = tensor_shape(input, useOldMetadata = true)

      val input_tensor = get_operand(input, anno)
      val output_tensor = get_operand(output, anno)
      val doutput_tensor = get_operand(doutput, anno)

      val input_descriptor = getTensorDescriptor(input_shape, "tensor")
      val activation_descriptor = getActivationDescriptor(mode, coef)

      val size = numeral(input_shape)
      val dinput = withComment(s"allocating gpu array of size $size and type Float for the gradient input of activation") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("activation backward pass") {
        CUDNN_CHECK(CUDNN_ACTIVATION_BWD(myCUDNNComm, activation_descriptor, VAR(FLOAT(alpha)), input_descriptor, new ARRAY(output_tensor),
          input_descriptor, new ARRAY(doutput_tensor), input_descriptor, new ARRAY(input_tensor), VAR(FLOAT(beta)), input_descriptor,
          dinput))
      }
      dinput.x

    case Node(s, "tensors_dropout", Backend.Const(tts: List[TensorType])::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack dropout paratemers
      val DropoutParam(dropout, seed) = params.asInstanceOf[DropoutParam]

      // get input info and transform input tensors
      val input_shape = tensor_shape(input, useOldMetadata = true)
      val input_tensor = get_operand(input, anno)

      val input_descriptor = getTensorDescriptor(input_shape, "tensor")
      val output_descriptor = getTensorDescriptor(input_shape, "tensor")

      generate_comment("begin finding dropout forward reserve space bytes")
      var reserve_bytes = VAR(SIZE_T(0))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_RESERVE_SPACE_SZ(input_descriptor, reserve_bytes))
      generate_comment("end finding dropout forward reserve space bytes")

      generate_comment("begin finding dropout forward states bytes")
      var states_bytes = VAR(SIZE_T(0))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_STATES_SZ(myCUDNNComm, states_bytes))
      generate_comment("end finding dropout forward states bytes")

      val d_reservespace = withComment("allocating gpu array for the reserve space of dropout forward") {
        GPU_ARRAY_BY_BYTE(INT(reserve_bytes(pos)), manifest[Float], myCUDADevice)
      }

      val d_states = withComment("allocating gpu array for the states of dropout forward") {
        GPU_ARRAY_BY_BYTE(INT(states_bytes(pos)), manifest[Float], myCUDADevice)
      }

      val dropout_descriptor = getDropoutDescriptor(d_states, states_bytes, params.asInstanceOf[DropoutParam])

      // allocate output tensor
      val size = numeral(input_shape)
      val output = withComment(s"allocating gpu array of size $size and type Float for the output of dropout") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("dropout forward pass") {
        CUDNN_CHECK(CUDNN_DROPOUT_FWD(myCUDNNComm, dropout_descriptor, input_descriptor, new ARRAY(input_tensor),
          output_descriptor, output, d_reservespace, reserve_bytes))
      }

      // return dropout output
      Adapter.g.reflect("tuple-view", output.x, d_reservespace.x)

    case Node(s, "tensor_dropout_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(doutput:Backend.Sym)::
      (reserveSpace:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack dropout paratemers
      val DropoutParam(dropout, seed) = params.asInstanceOf[DropoutParam]

      val doutput_shape = tensor_shape(doutput, useOldMetadata = true)
      val doutput_descriptor = getTensorDescriptor(doutput_shape, "tensor")
      val dinput_descriptor = doutput_descriptor

      val doutput_tensor = get_operand(doutput, anno)
      val reserveSpace_tensor = get_operand(reserveSpace, anno)

      val size = numeral(doutput_shape)

      val dinput = withComment(s"allocating gpu array of size $size and type Float for the gradient input of dropout") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      generate_comment("begin finding dropout backward reserve bytes")
      var reserve_bytes = VAR(SIZE_T(0))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_RESERVE_SPACE_SZ(doutput_descriptor, reserve_bytes))
      generate_comment("end finding dropout backward reserve bytes")

      generate_comment("begin finding dropout backward states bytes")
      var states_bytes = VAR(SIZE_T(0))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_STATES_SZ(myCUDNNComm, states_bytes))
      generate_comment("end finding dropout backward states bytes")

      val d_states = withComment("allocating gpu array for the states of dropout backward") {
        GPU_ARRAY_BY_BYTE(INT(states_bytes(pos)), manifest[Float], myCUDADevice)
      }

      val dropout_descriptor = getDropoutDescriptor(d_states, states_bytes, params.asInstanceOf[DropoutParam])

      withComment("dropout backward pass") {
        CUDNN_CHECK(CUDNN_DROPOUT_BWD(myCUDNNComm, dropout_descriptor, doutput_descriptor, new ARRAY(doutput_tensor), dinput_descriptor,
          dinput, new ARRAY(reserveSpace_tensor), reserve_bytes))
      }
      dinput.x

    case Node(s, "tensor_pooling", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      Backend.Const(params)::Backend.Const(mode:String)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack pooling paratemers
      val PoolingParam(alpha, beta, window, padding, strides) = params.asInstanceOf[PoolingParam]

      val input_shape = tensor_shape(input, useOldMetadata = true)
      val output_shape = tensor_shape(s, useOldMetadata = true)

      val input_tensor = get_operand(input, anno)

      val input_descriptor = getTensorDescriptor(input_shape, "tensor")
      val output_descriptor = getTensorDescriptor(output_shape, "tensor")
      val pooling_descriptor = getPoolingDescriptor(mode, window, padding, strides)

      // allocate output tensor
      val size = numeral(output_shape)

      val output = withComment(s"allocating gpu array of size $size and type Float for the output of pooling") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("pooling forward pass") {
        CUDNN_CHECK(CUDNN_POOLING_FWD(myCUDNNComm, pooling_descriptor, VAR(FLOAT(alpha)), input_descriptor, new ARRAY(input_tensor),
          VAR(FLOAT(beta)), output_descriptor, output))
      }
      output.x

    case Node(s, "tensor_pooling_bwd",Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::(output:Backend.Sym)::
      (doutput:Backend.Sym)::Backend.Const(params)::Backend.Const(mode:String)::_, _) =>

      implicit val pos = Adapter.oldSourceMap(s)

      // unpack pooling paratemers
      val PoolingParam(alpha, beta, window, padding, strides) = params.asInstanceOf[PoolingParam]

      val doutput_shape = tensor_shape(doutput, useOldMetadata = true)
      val input_shape = tensor_shape(input, useOldMetadata = true)

      val output_tensor = get_operand(output, anno)
      val doutput_tensor = get_operand(doutput, anno)
      val input_tensor = get_operand(input, anno)

      val doutput_descriptor = getTensorDescriptor(doutput_shape, "tensor")
      val output_descriptor = doutput_descriptor
      val input_descriptor = getTensorDescriptor(input_shape, "tensor")

      val pooling_descriptor = getPoolingDescriptor(mode, window, padding, strides)

      val size = numeral(input_shape)

      val dinput = withComment(s"allocating gpu array of size $size and type Float for the gradient input of pooling") {
        gpu_array(size, manifest[Float], myCUDADevice)
      }

      withComment("pooling backward pass") {
        CUDNN_CHECK(CUDNN_POOLING_BWD(myCUDNNComm, pooling_descriptor, VAR(FLOAT(alpha)), output_descriptor, new ARRAY(output_tensor),
          doutput_descriptor, new ARRAY(doutput_tensor), input_descriptor, new ARRAY(input_tensor), VAR(FLOAT(beta)), input_descriptor,
          dinput))
      }
      dinput.x

    case _ => super.transform(n)
  }


}
