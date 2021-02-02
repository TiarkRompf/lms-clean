package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps,CUDNNTypeLess,CLibTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._


trait DistributeTensor2MPI_NCCLConv extends DistributeTensor2MPI_NCCLBase {

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


  // hashmap: tensor -> (kind, descriptor)

  // get_descriptor(tensor, kind)
  // if tensor in hashmap, return hashmap.get(tensor)
  // else, create and set descriptor based on kind, return descriptor

  // clean_up
  // forall tensor in hashmap, destroy tensor according to its kind
  
  def get_descriptor(t: Backend.Sym, kind: String): TOP = cudnnTensor2Desc.get(t) match {
    case Some((desc, _)) => desc
    case None => 
      val shape = tensor_shape(t, useOldMetadata = true)
      // val n = shape(0)
      // val h = shape(1)
      // val w = shape(2)
      // val c = shape(3)
      val n::h::w::c::_ = shape
      kind match {
        case "tensor" =>
          val desc = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_TENSOR_DESCRIPTOR], "cudnnTensorDescriptor_t").x)
          CUDNN_CREATE_TENSOR_DESCRIPTOR(desc)
          CUDNN_SET_TENSOR_4D_DESCRIPTOR(desc, CUDNN_NHWC, CUDNN_FLOAT, n, c, h, w)
          cudnnTensor2Desc += ((t, (desc, kind)))
          desc
        case "filter" =>
          val desc = new CUDNN_FILTER_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_FILTER_DESCRIPTOR], "cudnnFilterDescriptor_t").x)
          CUDNN_CREATE_FILTER_DESCRIPTOR(desc)
          CUDNN_SET_FILTER_4D_DESCRIPTOR(desc, CUDNN_NHWC, CUDNN_FLOAT, n, c, h, w)
          cudnnTensor2Desc += ((t, (desc, kind)))
          desc
        case _ => throw new Exception("Unknown kind of cudnn tensor descriptor")
      }
  }


  override def transform(n: Node): Backend.Exp = n match {
    // NHWC as default layout
    case Node(s, "cudnn_conv", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::
      Backend.Const(padding:Seq[Int])::Backend.Const(strides:Seq[Int])::Backend.Const(dilation:Seq[Int])::
      Backend.Const(alpha:Float)::Backend.Const(beta:Float)::_, _) =>
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

      // inputs
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      val leftTensor = new TENSOR(left_operand, useOldMetadata = true)
      val rightTensor = new TENSOR(right_operand, useOldMetadata = true)
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      // assertions
      assert(left_shape.size != tensor_dim, "input tensor of convolution must be 4D")
      assert(right_shape.size != tensor_dim, "input filter of convolution must be 4D")
      assert(padding.size != hyperparam_dim, "padding must be sequence of integer of length 2")
      assert(strides.size != hyperparam_dim, "strides must be sequence of integer of length 2")
      assert(dilation.size != hyperparam_dim, "dilation must be sequence of integer of length 2")

      // create input descriptor
      /*
      val input_batchsize = left_shape(0)
      val input_height = left_shape(1)
      val input_width = left_shape(2)
      val input_channels = left_shape(3)
      val input_descriptor = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_TENSOR_DESCRIPTOR], "cudnnTensorDescriptor_t").x)
      CUDNN_CREATE_TENSOR_DESCRIPTOR(input_descriptor)
      CUDNN_SET_TENSOR_4D_DESCRIPTOR(input_descriptor, layout, datatype, 
        input_batchsize, input_channels, input_height, input_width)
      */
      val input_descriptor = get_descriptor(left, "tensor")

      // create filter descriptor
      /*
      val filter_out_channels = right_shape(0)
      val filter_height = right_shape(1)
      val filter_width = right_shape(2)
      val filter_in_channels = right_shape(3)
      val filter_descriptor = new CUDNN_FILTER_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_FILTER_DESCRIPTOR], "cudnnFilterDescriptor_t").x)
      CUDNN_CREATE_FILTER_DESCRIPTOR(filter_descriptor)
      CUDNN_SET_FILTER_4D_DESCRIPTOR(filter_descriptor, layout, datatype, 
        filter_out_channels, filter_in_channels, filter_height, filter_width)
      */
      val filter_descriptor = get_descriptor(right, "filter")

      // create convolution descriptor
      val conv_descriptor = new CUDNN_CONV_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
      CUDNN_CREATE_CONV_DESCRIPTOR(conv_descriptor)
      CUDNN_SET_CONV_2D_DESCRIPTOR(conv_descriptor, padding(0), padding(1), strides(0), strides(1), dilation(0), dilation(1),
        mode, datatype)

      // find output tensor shape
      var output_batchsize = 0
      var output_height = 0
      var output_width = 0
      var output_channels = 0
      CUDNN_GET_CONV_2D_FWD_OUTPUT_DIM(conv_descriptor, input_descriptor, filter_descriptor, 
        output_batchsize, output_channels, output_height, output_width)

      // create output descriptor
      val output_descriptor = new CUDNN_TENSOR_DESCRIPTOR(NEW_STRUCT(manifest[CUDNN_CONV_DESCRIPTOR], "cudnnConvolutionDescriptor_t").x)
      CUDNN_CREATE_TENSOR_DESCRIPTOR(output_descriptor)
      CUDNN_SET_TENSOR_4D_DESCRIPTOR(output_descriptor, layout, datatype, 
        output_batchsize, output_channels, output_height, output_width)
      // val output_descriptor = get_descriptor(s, "tensor")  // TODO: use get_descriptor for output_descriptor

      // allocate output tensor
      val output_size = output_batchsize * output_height * output_width * output_channels
      val output = gpu_array(output_size, manifest[Float], myNCCLRank)
      
      // find convolution algorithm
      var res_count = 0
      val res = new CUDNN_CONV_FWD_ALG_PERF(NEW_STRUCT(manifest[CUDNN_CONV_FWD_ALG_PERF], "cudnnConvolutionFwdAlgoPerf_t").x)
      CUDNN_FIND_CONV_FWD_ALG(myCUDNNComm, input_descriptor, filter_descriptor, conv_descriptor, output_descriptor, 1, res_count, res)
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
      CUDNN_CONV_FWD(myCUDNNComm, VAR(alpha), input_descriptor, leftTensor, filter_descriptor, rightTensor, conv_descriptor, convAlgo, d_workspace, 
        SIZE_T(workspace_bytes), VAR(beta), output_descriptor, sourceTensor)

      // return convolution output
      output.x

      /* Typed interface (not used)
      val sourceTensor = new TENSOR(s, useOldMetadata = true).x

      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)

      val image_bytes = 1 * 1 * left_shape(0) * left_shape(1)

      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      val leftTensor = new TENSOR(left_operand, useOldMetadata = true)
      val rightTensor = new TENSOR(right_operand, useOldMetadata = true)

      val input_descriptor = cudnnTensorDescriptor
      cudnnCheck(cudnnCreateTensorDescriptor(input_descriptor))
      cudnnSetTensor4dDescriptor(input_descriptor, cudnnNCHW, cudnnFloat, unit[Int](1), unit[Int](1), unit[Int](left_shape(0)), unit[Int](left_shape(1)))

      val filter_descriptor = cudnnFilterDescriptor
      cudnnCheck(cudnnCreateFilterDescriptor(filter_descriptor))
      cudnnCheck(cudnnSetFilter4dDescriptor(filter_descriptor, cudnnNCHW, cudnnFloat, unit[Int](1), unit[Int](1), unit[Int](right_shape(0)), unit[Int](right_shape(1))))

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

      Unwrap(cudnnCheck(cudnnConvolutionForward(myCUDNNCommRep, alpha, input_descriptor, Wrap[Array[Float]](left_operand), 
          filter_descriptor, right_operand, conv_descriptor, conv_algo, d_workspace, readVar(workspace_bytes), beta, 
          output_descriptor, sourceTensor)))
      */

      /* typeless READ_FIELD, not used
      def READ_FIELD(t: Manifest[_], u: Manifest[_], x: Backend.Exp, m: String)(implicit __pos: SourceContext): TOP = {
        TOP(Adapter.g.reflectUnsafe("read_field", x, Backend.Const(m)), u)
      }
      */

    case _ => super.transform(n) 
  }


}