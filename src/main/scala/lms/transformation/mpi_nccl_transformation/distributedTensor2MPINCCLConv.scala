package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps,CUDNNTypeLess,CLibTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.{DataStructure, ConvParam}

import Backend._


trait DistributeTensor2MPI_NCCLConv extends DistributeTensor2MPI_NCCLBase with ConvParam {

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


  // given a tensor, get its descriptor.
  // if desc already in hashmap, reuse it; Otherwise, create a new descriptor
  def get_descriptor(shape: Seq[Int], kind: String)(implicit __pos: SourceContext): TOP = cudnnTensor2Desc.get(shape) match {
    case Some((desc, _)) => desc
    case None => 
      val n::h::w::c::_ = shape
      kind match {
        case "tensor" =>
          val desc = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_TENSOR_DESCRIPTOR], "cudnnTensorDescriptor_t").x)
          CUDNN_CREATE_TENSOR_DESCRIPTOR(desc)
          CUDNN_SET_TENSOR_4D_DESCRIPTOR(desc, CUDNN_NHWC, CUDNN_FLOAT, INT(n), INT(c), INT(h), INT(w))
          cudnnTensor2Desc += ((shape, (desc, kind)))
          desc
        case "filter" =>
          val desc = new CUDNN_FILTER_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_FILTER_DESCRIPTOR], "cudnnFilterDescriptor_t").x)
          CUDNN_CREATE_FILTER_DESCRIPTOR(desc)
          CUDNN_SET_FILTER_4D_DESCRIPTOR(desc, CUDNN_NHWC, CUDNN_FLOAT, INT(n), INT(c), INT(h), INT(w))
          cudnnTensor2Desc += ((shape, (desc, kind)))
          desc
        case _ => throw new Exception("Unknown kind of cudnn tensor descriptor")
      }
  }

  
  def get_conv_descriptor(padding: Seq[Int], strides: Seq[Int], dilation: Seq[Int])(implicit __pos: SourceContext): CUDNN_CONV_DESCRIPTOR = 
    cudnnConv2Desc.get(padding ++ strides ++ dilation) match {
      case Some(desc) => desc
      case None =>
        val desc = new CUDNN_CONV_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
        CUDNN_CREATE_CONV_DESCRIPTOR(desc)
        CUDNN_SET_CONV_2D_DESCRIPTOR(desc, INT(padding(0)), INT(padding(1)), INT(strides(0)), INT(strides(1)), INT(dilation(0)), INT(dilation(1)),
          CUDNN_CONVOLUTION, CUDNN_FLOAT)
        desc
    }
  

  override def transform(n: Node): Backend.Exp = n match {
    // NHWC as default layout
    case Node(s, "tensor_conv", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::
      Backend.Const(padding:Seq[Int])::Backend.Const(strides:Seq[Int])::Backend.Const(dilation:Seq[Int])::
      Backend.Const(alpha:Float)::Backend.Const(beta:Float)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      // these are default settings
      // TODO: perhaps move to base?
      val tensor_dim = 4                // input tensor and input kernel should both be 4d
      val hyperparam_dim = 2            // degree of freedom of padding, strides, and dilation
      val layout = CUDNN_NHWC           // default tensor layout is batch x width x height x channel
      val datatype = CUDNN_FLOAT        // only consider float
      val mode = CUDNN_CONVOLUTION      // only consider convolution mode
      // val padding = Seq(1, 1)
      // val strides = Seq(1, 1)
      // val dilation = Seq(1, 1)
      // var alpha = 1.0f
      // var beta = 0.0f   

      // get input info and transform input tensors
      val weight_shape = tensor_shape(left, useOldMetadata = true)
      val filter_shape = tensor_shape(right, useOldMetadata = true)
      val weight_tensor = get_operand(left, anno)
      val filter_tensor = get_operand(right, anno)

      // assertions
      assert(weight_shape.size != tensor_dim, "input tensor of convolution must be 4D")
      assert(filter_shape.size != tensor_dim, "input filter of convolution must be 4D")
      assert(padding.size != hyperparam_dim, "padding must be sequence of integer of length 2")
      assert(strides.size != hyperparam_dim, "strides must be sequence of integer of length 2")
      assert(dilation.size != hyperparam_dim, "dilation must be sequence of integer of length 2")

      // create input descriptor
      /*
      val input_batchsize = weight_shape(0)
      val input_height = weight_shape(1)
      val input_width = weight_shape(2)
      val input_channels = weight_shape(3)
      val input_descriptor = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_TENSOR_DESCRIPTOR], "cudnnTensorDescriptor_t").x)
      CUDNN_CREATE_TENSOR_DESCRIPTOR(input_descriptor)
      CUDNN_SET_TENSOR_4D_DESCRIPTOR(input_descriptor, layout, datatype, 
        input_batchsize, input_channels, input_height, input_width)
      */
      val input_descriptor = get_descriptor(weight_shape, "tensor")

      // create filter descriptor
      /*
      val filter_out_channels = filter_shape(0)
      val filter_height = filter_shape(1)
      val filter_width = filter_shape(2)
      val filter_in_channels = filter_shape(3)
      val filter_descriptor = new CUDNN_FILTER_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_FILTER_DESCRIPTOR], "cudnnFilterDescriptor_t").x)
      CUDNN_CREATE_FILTER_DESCRIPTOR(filter_descriptor)
      CUDNN_SET_FILTER_4D_DESCRIPTOR(filter_descriptor, layout, datatype, 
        filter_out_channels, filter_in_channels, filter_height, filter_width)
      */
      val filter_descriptor = get_descriptor(filter_shape, "filter")

      // create convolution descriptor
      /*
      val conv_descriptor = new CUDNN_CONV_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
      CUDNN_CREATE_CONV_DESCRIPTOR(conv_descriptor)
      CUDNN_SET_CONV_2D_DESCRIPTOR(conv_descriptor, INT(padding(0)), INT(padding(1)), INT(strides(0)), INT(strides(1)), INT(dilation(0)), INT(dilation(1)),
        mode, datatype)
      */
      val conv_descriptor = get_conv_descriptor(padding, strides, dilation)

      // find output tensor shape
      var output_batchsize = 0
      var output_height = 0
      var output_width = 0
      var output_channels = 0
      CUDNN_GET_CONV_2D_FWD_OUTPUT_DIM(conv_descriptor, input_descriptor, filter_descriptor, 
        INT(output_batchsize), INT(output_channels), INT(output_height), INT(output_width))

      // create output descriptor
      /*
      val output_descriptor = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
      CUDNN_CREATE_TENSOR_DESCRIPTOR(output_descriptor)
      CUDNN_SET_TENSOR_4D_DESCRIPTOR(output_descriptor, layout, datatype, 
        INT(output_batchsize), INT(output_channels), INT(output_height), INT(output_width))
      */
      val output_descriptor = get_descriptor(Seq(output_batchsize, output_height, output_width, output_channels), "tensor")

      // allocate output tensor
      val output_size = output_batchsize * output_height * output_width * output_channels
      val output = gpu_array(output_size, manifest[Float], myNCCLRank)
      
      // find convolution algorithm
      var res_count = 0
      val res = new CUDNN_CONV_FWD_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_FWD_ALG_PERF], "cudnnConvolutionFwdAlgoPerf_t").x)
      CUDNN_FIND_CONV_FWD_ALG(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor, INT(1), INT(res_count), res)
      // val conv_algo = READ_FIELD(manifest[CUDNN_CONV_FWD_ALG_PERF], manifest[CUDNN_CONV_FWD_ALGO], res.x, "algo")
      val convAlgoRep = readField[Manifest[CUDNN_CONV_FWD_ALG_PERF], Manifest[CUDNN_CONV_FWD_ALGO]](Wrap[Manifest[CUDNN_CONV_FWD_ALG_PERF]](res.x), "algo")
      val convAlgo = TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_FWD_ALGO])

      // allocate convolution workspace
      var workspace_bytes = 0
      CUDNN_GET_CONV_FWD_WORKSPACE_SZ(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor,
        convAlgo, SIZE_T(workspace_bytes))
      // var d_workspace = CUDA_MALLOC(workspace_bytes, manifest[FLOAT])
      val d_workspace = gpu_array(workspace_bytes, manifest[Float], myNCCLRank)

      // convolution
      CUDNN_CONV_FWD(myCUDNNComm, VAR(INT(alpha)), input_descriptor, new ARRAY(weight_tensor), filter_descriptor, new ARRAY(filter_tensor), 
        conv_descriptor, convAlgo, d_workspace, SIZE_T(workspace_bytes), VAR(INT(beta)), output_descriptor, output)

      // return convolution output
      output.x

      /*
      // Typed interface (not used)
      val sourceTensor = new TENSOR(s, useOldMetadata = true).x

      val weight_shape = tensor_shape(left, useOldMetadata = true)
      val filter_shape = tensor_shape(right, useOldMetadata = true)

      val image_bytes = 1 * 1 * weight_shape(0) * weight_shape(1)

      val weight_tensor = get_operand(left, anno)
      val filter_tensor = get_operand(right, anno)
      val leftTensor = new TENSOR(weight_tensor, useOldMetadata = true)
      val rightTensor = new TENSOR(filter_tensor, useOldMetadata = true)

      val input_descriptor = cudnnTensorDescriptor
      cudnnCheck(cudnnCreateTensorDescriptor(input_descriptor))
      cudnnSetTensor4dDescriptor(input_descriptor, cudnnNCHW, cudnnFloat, unit[Int](1), unit[Int](1), unit[Int](weight_shape(0)), unit[Int](weight_shape(1)))

      val filter_descriptor = cudnnFilterDescriptor
      cudnnCheck(cudnnCreateFilterDescriptor(filter_descriptor))
      cudnnCheck(cudnnSetFilter4dDescriptor(filter_descriptor, cudnnNCHW, cudnnFloat, unit[Int](1), unit[Int](1), unit[Int](filter_shape(0)), unit[Int](filter_shape(1))))

      val conv_descriptor = cudnnConvolutionDescriptor
      cudnnCheck(cudnnCreateConvolutionDescriptor(conv_descriptor))
      cudnnCheck(cudnnSetConvolution2dDescriptor(conv_descriptor, unit[Int](1), unit[Int](1), unit[Int](1), unit[Int](1), unit[Int](1), unit[Int](1), 
        cudnnConvolution, cudnnFloat))
      
      val batch_size = var_new(unit(0))
      val channels = var_new(unit(0))
      val height = var_new(unit(0))
      val width = var_new(unit(0))
      cudnnCheck(cudnnGetConvolution2dForwardOutputDim(conv_descriptor, input_descriptor, filter_descriptor, batch_size, channels, height, width))

      val output_descriptor = cudnnTensorDescriptor
      cudnnCheck(cudnnCreateTensorDescriptor(output_descriptor))
      cudnnCheck(cudnnSetTensor4dDescriptor(output_descriptor, cudnnNCHW, cudnnFloat, unit[Int](1), unit[Int](1), readVar(height), readVar(width)))

      val res_count = var_new(unit(0))
      val res = cudnnConvolutionFwdAlgoPerf
      cudnnCheck(cudnnFindConvolutionForwardAlgorithm(myCUDNNCommRep, input_descriptor, filter_descriptor, conv_descriptor,
          output_descriptor, unit[Int](1), res_count, res))
      val conv_algo = readField[cudnnConvolutionFwdAlgoPerfT, cudnnConvolutionFwdAlgoT](res, "algo")

      var workspace_bytes = var_new[SizeT](unit(0))
      cudnnCheck(cudnnGetConvolutionForwardWorkspaceSize(myCUDNNCommRep, input_descriptor, filter_descriptor, conv_descriptor,
          output_descriptor, conv_algo, workspace_bytes))
      
      
      var d_workspace = cudaMalloc2BySize[Float](readVar(workspace_bytes))

      val alpha = var_new[Float](unit(1.0))
      val beta = var_new[Float](unit(0.0))

      Unwrap(cudnnCheck(cudnnConvolutionForward(myCUDNNCommRep, alpha, input_descriptor, Wrap[Array[Float]](weight_tensor), 
          filter_descriptor, filter_tensor, conv_descriptor, conv_algo, d_workspace, readVar(workspace_bytes), beta, 
          output_descriptor, sourceTensor)))
      */

      /* typeless READ_FIELD, not used
      def READ_FIELD(t: Manifest[_], u: Manifest[_], x: Backend.Exp, m: String)(implicit __pos: SourceContext): TOP = {
        TOP(Adapter.g.reflectUnsafe("read_field", x, Backend.Const(m)), u)
      }
      */
    
    
    // inputs: 
    // alpha, beta
    // wDesc (filter descriptor)
    // w (filter tensor array)
    // convDesc
    // 
    // creates:
    // dyDesc (tensor descriptor of conv output tensor)
    // dy
    // algo: cudnnConvolutionBwdDataAlgo_t
    // workspace, workspace_size: from cudnnGetConvolutionBackwardDataWorkspaceSize()
    // dx (retval)
    case Node(s, "tensor_conv_bwd_data", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::
      (weight:Backend.Sym)::(filter:Backend.Sym)::(doutput:Backend.Sym)::Backend.Const(params:ConvParam)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      // unpack convolution paratemers
      val padding = params.padding
      val strides = params.strides
      val dilation = params.dilation
      val alpha = params.alpha
      val beta = params.beta

      // TODO: dim checks necessary or not?
      val doutput_shape = tensor_shape(s, useOldMetadata = true)
      val weight_shape = tensor_shape(s, useOldMetadata = true)
      val filter_shape = tensor_shape(s, useOldMetadata = true)

      val filter_tensor = get_operand(filter, anno)
      val output_tensor = get_operand(doutput, anno)

      val filter_descriptor = get_descriptor(filter_shape, "filter")
      val doutput_descriptor = get_descriptor(doutput_shape, "tensor")
      val dweight_descriptor = get_descriptor(weight_shape, "tensor")  // should this descriptor just be weight descriptor?
      val conv_descriptor = get_conv_descriptor(padding, strides, dilation)

      // allocate result (d_weight) 
      val dweight_size = weight_shape(0) * weight_shape(1) * weight_shape(2) * weight_shape(3)
      val dweight = gpu_array(dweight_size, manifest[Float], myNCCLRank)
      
      // find convolution algorithm
      var res_count = 0
      val res = new CUDNN_CONV_BWD_DATA_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_BWD_DATA_ALG_PERF], "cudnnConvolutionBwdDataAlgoPerf_t").x)
      CUDNN_FIND_CONV_BWD_DATA_ALG(myCUDNNComm, filter_descriptor, doutput_descriptor, conv_descriptor, dweight_descriptor, INT(1), INT(res_count), res)
      val convAlgoRep = readField[Manifest[CUDNN_CONV_BWD_DATA_ALG_PERF], Manifest[CUDNN_CONV_BWD_DATA_ALGO]](Wrap[Manifest[CUDNN_CONV_BWD_DATA_ALG_PERF]](res.x), "algo")
      val convAlgo = TOP(Unwrap(convAlgoRep), manifest[CUDNN_CONV_BWD_DATA_ALGO])
      
      // allocate convolution workspace
      var workspace_bytes = 0
      CUDNN_GET_CONV_BWD_DATA_WORKSPACE_SZ(myCUDNNComm, filter_descriptor, doutput_descriptor, conv_descriptor, dweight_descriptor,
        convAlgo, SIZE_T(workspace_bytes))
      val d_workspace = gpu_array(workspace_bytes, manifest[Float], myNCCLRank)

      // backward data pass
      CUDNN_CONV_BWD_DATA(myCUDNNComm, VAR(INT(alpha)), filter_descriptor, new ARRAY(filter_tensor), doutput_descriptor, new ARRAY(output_tensor),
        conv_descriptor, convAlgo, d_workspace, SIZE_T(workspace_bytes), VAR(INT(beta)), dweight_descriptor, dweight)
      
      dweight.x

    
    case _ => super.transform(n) 
  }


}
