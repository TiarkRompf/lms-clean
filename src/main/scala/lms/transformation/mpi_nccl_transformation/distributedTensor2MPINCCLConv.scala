package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.ListBuffer

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps,CUDNNTypeLess,CLibTypeLess}
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
  def getConvDescriptor(padding: Seq[Int], strides: Seq[Int], dilation: Seq[Int])(implicit __pos: SourceContext): CUDNN_CONV_DESCRIPTOR =
    cudnnConv2Desc.get(padding ++ strides ++ dilation) match {
      case Some(desc) => desc
      case None =>
        generate_comment(s"begin creating and setting convolution descriptor of padding: ${padding}, strides: ${strides}, dilation: ${dilation}")
        val desc = new CUDNN_CONV_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
        CUDNN_CHECK(CUDNN_CREATE_CONV_DESCRIPTOR(desc))
        CUDNN_CHECK(CUDNN_SET_CONV_2D_DESCRIPTOR(desc,
          INT(padding(CUDNN_PARAM_H)),  INT(padding(CUDNN_PARAM_W)),
          INT(strides(CUDNN_PARAM_H)),  INT(strides(CUDNN_PARAM_W)),
          INT(dilation(CUDNN_PARAM_H)), INT(dilation(CUDNN_PARAM_W)),
          CUDNN_CONVOLUTION, CUDNN_FLOAT))
        generate_comment(s"end creating and setting convolution descriptor")
        desc
  }


  override def transform(n: Node): Backend.Exp = n match {
    // convolution forward operation
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

      generate_comment("begin finding convolution output tensor shape")
      var output_batchsize = 0
      var output_height = 0
      var output_width = 0
      var output_channels = 0
      CUDNN_CHECK(CUDNN_GET_CONV_2D_FWD_OUTPUT_DIM(conv_descriptor, input_descriptor, filter_descriptor,
        INT(output_batchsize), INT(output_channels), INT(output_height), INT(output_width)))
      generate_comment("end finding convolution output tensor shape")

      val output_shape: ListBuffer[Int] = ListBuffer(0, 0, 0, 0)
      output_shape(CUDNN_N) = output_batchsize
      output_shape(CUDNN_C) = output_channels
      output_shape(CUDNN_H) = output_height
      output_shape(CUDNN_W) = output_width
      val output_descriptor = getTensorDescriptor(output_shape.toList, "tensor")

      // allocate output tensor
      generate_comment("begin allocating gpu array for the output of convolution")
      val output_size = output_batchsize * output_height * output_width * output_channels
      val output = gpu_array(output_size, manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the output of convolution")

      generate_comment("begin finding convolution forward algorithm")
      var res_count = 0
      val res = new CUDNN_CONV_FWD_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_FWD_ALG_PERF], "cudnnConvolutionFwdAlgoPerf_t").x)
      CUDNN_CHECK(CUDNN_FIND_CONV_FWD_ALG(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor, INT(1), VAR(res_count), res))
      val convAlgoRep = readField[Manifest[CUDNN_CONV_FWD_ALG_PERF], Manifest[CUDNN_CONV_FWD_ALGO]](Wrap[Manifest[CUDNN_CONV_FWD_ALG_PERF]](res.x), "algo")
      val convAlgo = TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_FWD_ALGO])
      generate_comment("end finding convolution forward algorithm")

      generate_comment("begin finding convolution forward workspace size")
      var workspace_bytes = 0
      val workspace_bytes_v = VAR(SIZE_T(workspace_bytes)) // var read
      CUDNN_CHECK(CUDNN_GET_CONV_FWD_WORKSPACE_SZ(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor,
        convAlgo, workspace_bytes_v))
      generate_comment("begin finding convolution backward workspace size")

      generate_comment("begin allocating gpu array for convolution forward workspace")
      val d_workspace = gpu_array1(INT(workspace_bytes_v(pos)), manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for convolution forward workspace")

      generate_comment("begin convolution forward pass")
      CUDNN_CHECK(CUDNN_CONV_FWD(myCUDNNComm, VAR(FLOAT(alpha)), input_descriptor, new ARRAY(weight_tensor), filter_descriptor, new ARRAY(filter_tensor),
        conv_descriptor, convAlgo, d_workspace, workspace_bytes_v, VAR(FLOAT(beta)), output_descriptor, output))
      generate_comment("begin convolution forward pass")

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

      generate_comment("begin allocating gpu array for the gradient of weight of convolution")
      val dweight_size = weight_shape(0) * weight_shape(1) * weight_shape(2) * weight_shape(3)
      val dweight = gpu_array(dweight_size, manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the gradient of weight of convolution")

      generate_comment("begin finding convolution backward data algorithm")
      var res_count = 0
      val res = new CUDNN_CONV_BWD_DATA_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_BWD_DATA_ALG_PERF], "cudnnConvolutionBwdDataAlgoPerf_t").x)
      CUDNN_CHECK(CUDNN_FIND_CONV_BWD_DATA_ALG(myCUDNNComm, filter_descriptor, doutput_descriptor, conv_descriptor, dweight_descriptor, INT(1), VAR(res_count), res))
      val convAlgoRep = readField[Manifest[CUDNN_CONV_BWD_DATA_ALG_PERF], Manifest[CUDNN_CONV_BWD_DATA_ALGO]](Wrap[Manifest[CUDNN_CONV_BWD_DATA_ALG_PERF]](res.x), "algo")
      val convAlgo = TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_BWD_DATA_ALGO])
      generate_comment("end finding convolution backward data algorithm")

      generate_comment("begin finding convolution backward data workspace size")
      var workspace_bytes = 0
      val workspace_bytes_v = VAR(SIZE_T(workspace_bytes)) // var read
      CUDNN_CHECK(CUDNN_GET_CONV_BWD_DATA_WORKSPACE_SZ(myCUDNNComm, filter_descriptor, doutput_descriptor, conv_descriptor, dweight_descriptor,
        convAlgo, workspace_bytes_v))
      generate_comment("end finding convolution backward data workspace size")

      generate_comment("begin allocating gpu array for convolution backward data workspace")
      val d_workspace = gpu_array1(INT(workspace_bytes_v(pos)), manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for convolution backward data workspace")

      generate_comment("begin convolution backward data pass")
      CUDNN_CHECK(CUDNN_CONV_BWD_DATA(myCUDNNComm, VAR(FLOAT(alpha)), filter_descriptor, new ARRAY(filter_tensor), doutput_descriptor, new ARRAY(output_tensor),
        conv_descriptor, convAlgo, d_workspace, workspace_bytes_v, VAR(FLOAT(beta)), dweight_descriptor, dweight))
      generate_comment("end convolution backward data pass")

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

      generate_comment("begin allocating gpu array for the gradient of filter of convolution")
      val dfilter_size = filter_shape(0) * filter_shape(1) * filter_shape(2) * filter_shape(3)
      val dfilter = gpu_array(dfilter_size, manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the gradient of filter of convolution")

      generate_comment("begin finding convolution backward filter algorithm") 
      var res_count = 0
      val res = new CUDNN_CONV_BWD_FILTER_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_BWD_FILTER_ALG_PERF], "cudnnConvolutionBwdFilterAlgoPerf_t").x)
      CUDNN_CHECK(CUDNN_FIND_CONV_BWD_FILTER_ALG(myCUDNNComm, weight_descriptor, doutput_descriptor, conv_descriptor, dfilter_descriptor, INT(1), VAR(INT(res_count)), res))
      val convAlgoRep = readField[Manifest[CUDNN_CONV_BWD_FILTER_ALG_PERF], Manifest[CUDNN_CONV_BWD_FILTER_ALGO]](Wrap[Manifest[CUDNN_CONV_BWD_FILTER_ALG_PERF]](res.x), "algo")
      val convAlgo = TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_BWD_FILTER_ALGO])
      generate_comment("end finding convolution backward filter algorithm")

      generate_comment("begin finding convolution backward filter workspace size")
      var workspace_bytes = 0
      val workspace_bytes_v = VAR(SIZE_T(workspace_bytes)) // var read
      CUDNN_CHECK(CUDNN_GET_CONV_BWD_FILTER_WORKSPACE_SZ(myCUDNNComm, weight_descriptor, doutput_descriptor, conv_descriptor, dfilter_descriptor,
        convAlgo, workspace_bytes_v))
      generate_comment("end finding convolution backward filter workspace size")

      generate_comment("begin allocating gpu array for convolution backward filter workspace")
      val d_workspace = gpu_array1(INT(workspace_bytes_v(pos)), manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for convolution backward filter workspace")
     
      generate_comment("begin convolution backward filter pass")
      CUDNN_CHECK(CUDNN_CONV_BWD_FILTER(myCUDNNComm, VAR(FLOAT(alpha)), weight_descriptor, new ARRAY(weight_tensor), doutput_descriptor, new ARRAY(doutput_tensor),
        conv_descriptor, convAlgo, d_workspace, workspace_bytes_v, VAR(FLOAT(beta)), dfilter_descriptor, dfilter))
      generate_comment("end convolution backward filter pass")

      dfilter.x
    
    case Node(s, "tensor_dropout", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::
      Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack dropout paratemers
      val DropoutParam(dropout, seed) = params.asInstanceOf[DropoutParam]

      // get input info and transform input tensors
      val input_shape = tensor_shape(input, useOldMetadata = true)
      val input_tensor = get_operand(input, anno)

      val input_descriptor = getTensorDescriptor(input_shape, "tensor")
      val output_descriptor = getTensorDescriptor(input_shape, "tensor")

      generate_comment("begin finding dropout forward reserve bytes") 
      var reserve_bytes = 0
      var reserve_bytes_v = VAR(SIZE_T(reserve_bytes))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_RESERVE_SPACE_SZ(input_descriptor, reserve_bytes_v))
      generate_comment("end finding dropout forward reserve bytes") 

      generate_comment("begin finding dropout forward states bytes")
      var states_bytes = 0
      var states_bytes_v = VAR(SIZE_T(reserve_bytes))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_STATES_SZ(myCUDNNComm, states_bytes_v))
      generate_comment("end finding dropout forward states bytes")

      generate_comment("begin allocating gpu array for the reserve space of dropout forward")
      val d_reservespace = gpu_array1(INT(reserve_bytes_v(pos)), manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the reserve space of dropout forward")

      generate_comment("begin allocating gpu array for the states of dropout forward")
      val d_states = gpu_array1(INT(states_bytes_v(pos)), manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the states of dropout forward")

      generate_comment("begin creating dropout descriptor")
      val dropout_descriptor = new CUDNN_DROPOUT_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_DROPOUT_DESCRIPTOR], "cudnnDropoutDescriptor_t").x)
      CUDNN_CHECK(CUDNN_CREATE_DROPOUT_DESCRIPTOR(dropout_descriptor))
      CUDNN_CHECK(CUDNN_SET_DROPOUT_DESCRIPTOR(dropout_descriptor, myCUDNNComm, dropout, d_states, states_bytes_v, seed))
      generate_comment("end creating dropout descriptor")

      // allocate output tensor
      // output tensor has the same shape as input tensor
      generate_comment("begin allocating gpu array for the output of dropout")
      val output_size = input_shape(0) * input_shape(1) * input_shape(2) * input_shape(3)
      val output = gpu_array(output_size, manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the output of dropout")

      generate_comment("begin dropout forward pass")
      CUDNN_DROPOUT_FWD(myCUDNNComm, dropout_descriptor, input_descriptor, new ARRAY(input_tensor), 
        output_descriptor, output, d_reservespace, reserve_bytes_v)
      generate_comment("end dropout forward pass")

      // return dropout output
      // Adapter.g.reflect("tuple-view", output.x, d_states.x, d_reservespace.x)
      output.x

    
    case Node(s, "tensor_dropout_bwd", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(doutput:Backend.Sym)::
      (state:Backend.Sym)::(reserveSpace:Backend.Sym)::Backend.Const(params)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack dropout paratemers
      val DropoutParam(dropout, seed) = params.asInstanceOf[DropoutParam]

      val doutput_shape = tensor_shape(doutput, useOldMetadata = true)
      val doutput_descriptor = getTensorDescriptor(doutput_shape, "tensor")
      val dinput_descriptor = doutput_descriptor
      
      generate_comment("begin allocating gpu array for the gradient of input of dropout")
      val dinput_size = doutput_shape(0) * doutput_shape(1) * doutput_shape(2) * doutput_shape(3)
      val dinput = gpu_array(dinput_size, manifest[Float], myNCCLRank)
      generate_comment("end allocating gpu array for the gradient of input of dropout")

      generate_comment("begin finding dropout backward reserve bytes") 
      var reserve_bytes = 0
      var reserve_bytes_v = VAR(SIZE_T(reserve_bytes))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_RESERVE_SPACE_SZ(doutput_descriptor, reserve_bytes_v))
      generate_comment("end finding dropout backward reserve bytes") 

      generate_comment("begin finding dropout backward states bytes")
      var states_bytes = 0
      var states_bytes_v = VAR(SIZE_T(reserve_bytes))  // var read
      CUDNN_CHECK(CUDNN_DROPOUT_GET_STATES_SZ(myCUDNNComm, states_bytes_v))
      generate_comment("end finding dropout backward states bytes")

      generate_comment("begin creating dropout descriptor")
      val dropout_descriptor = new CUDNN_DROPOUT_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_DROPOUT_DESCRIPTOR], "cudnnDropoutDescriptor_t").x)
      CUDNN_CHECK(CUDNN_CREATE_DROPOUT_DESCRIPTOR(dropout_descriptor))
      CUDNN_CHECK(CUDNN_SET_DROPOUT_DESCRIPTOR(dropout_descriptor, myCUDNNComm, dropout, new ARRAY(state), states_bytes_v, seed))
      generate_comment("end creating dropout descriptor")
      
      generate_comment("begin dropout backward pass")
      CUDNN_CHECK(CUDNN_DROPOUT_BWD(myCUDNNComm, dropout_descriptor, doutput_descriptor, new ARRAY(doutput), dinput_descriptor,
        dinput, new ARRAY(reserveSpace), reserve_bytes_v))
      generate_comment("end dropout backward pass")
      
      dinput.x
    

    case _ => super.transform(n)
  }


}
