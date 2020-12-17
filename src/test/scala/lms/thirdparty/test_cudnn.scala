package lms
package thirdparty

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
import lms.collection.mutable._
import lms.thirdparty.array_computation.{CUDATypeLess, CCodeGenCudaOps, CudaOps}

class CudnnTest extends TutorialFunSuite {
  val under = "thirdparty/cuda"

  abstract class DslDriverCCudnn[A: Manifest, B: Manifest] extends DslDriverC[A,B] with CudaOps with CUDNNOps with ArrayOps with SizeTOps { q =>
    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCudaOps with CCodeGenStackArray with CCodeGenSizeTOps with CCodeGenCUDNN {
      val IR: q.type = q
    }
    override val compilerCommand = "nvcc -std=c++11 -O3"

    val curPath = System.getProperty("user.dir")
    override val sourceFile = s"$curPath/snippet.cu"
    override val executable = s"$curPath/snippet"
  }

  test("test-conv") {
    // modified based on the following example:
    // https://gist.github.com/goldsborough/865e6717e64fbae75cdaf6c9914a130d
    val driver = new DslDriverCCudnn[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        // a simple input 9x9 image
        val img_row = 9
        val img_col = 9
        val image_bytes = 1 * 1 * img_col * img_col // batch_size * channels * height * width
        val img = NewArray[Float](image_bytes)
        for (i <- (0 until image_bytes): Rep[Range]) {
          img(i) = i
        }

        // an 3x3 kernel
        val h_kernel = NewArray[Float](3*3)
        h_kernel(0) = 1
        h_kernel(1) = 1
        h_kernel(2) = 1
        h_kernel(3) = 1
        h_kernel(4) = -7
        h_kernel(5) = 1
        h_kernel(6) = 1
        h_kernel(7) = 1
        h_kernel(8) = 1

        // set GPU device
        cudaCall(cudaSetDevice(0))

        // create handle, which serves as a sort of context object
        val cudnn = cudnnHandle
        cudnnCheck(cudnnCreate(cudnn))

        // describe the input tensor
        // 1 batch, 1 channel, image shape 9x9
        val input_descriptor = cudnnTensorDescriptor
        cudnnCheck(cudnnCreateTensorDescriptor(input_descriptor))
        cudnnCheck(cudnnSetTensor4dDescriptor(input_descriptor, cudnnNCHW, cudnnFloat, 1, 1, img_row, img_col))

        // describe the kernel tensor
        // one in_channel, one out_channel, kernel shape 3x3
        val kernel_descriptor = cudnnFilterDescriptor
        cudnnCheck(cudnnCreateFilterDescriptor(kernel_descriptor))
        cudnnCheck(cudnnSetFilter4dDescriptor(kernel_descriptor, cudnnNCHW, cudnnFloat, 1, 1, 3, 3))

        // describe the convolution kernel
        val conv_descriptor = cudnnConvolutionDescriptor
        cudnnCheck(cudnnCreateConvolutionDescriptor(conv_descriptor))
        cudnnCheck(cudnnSetConvolution2dDescriptor(conv_descriptor, 1, 1, 1, 1, 1, 1, cudnnConvolution, cudnnFloat))

        // find and print the output dimensions
        // really necessary for this example?
        var batch_size = 0
        var channels = 0
        var height = 0
        var width = 0
        cudnnCheck(cudnnGetConvolution2dForwardOutputDim(conv_descriptor, input_descriptor, kernel_descriptor, batch_size, channels, height, width))
        printf("Output Image: %d x %d x %d x %d", height, width, channels)



        // describe the output tensor
        // 1 batch, 1 channels, image shape 9x9
        val output_descriptor = cudnnTensorDescriptor
        cudnnCheck(cudnnCreateTensorDescriptor(output_descriptor))
        cudnnCheck(cudnnSetTensor4dDescriptor(output_descriptor, cudnnNCHW, cudnnFloat, 1, 1, img_row, img_col))

        // describe the convolution algorithm
        var res_count = 0
        val res = cudnnConvolutionFwdAlgoPerf
        cudnnCheck(cudnnFindConvolutionForwardAlgorithm(cudnn, input_descriptor, kernel_descriptor, conv_descriptor,
          output_descriptor, 1, res_count, res))
        val conv_algo = readField[cudnnConvolutionFwdAlgoPerfT, cudnnConvolutionFwdAlgoT](res, "algo")

        // ask CuDNN for memory usage
        var workspace_bytes = 0
        cudnnCheck(cudnnGetConvolutionForwardWorkspaceSize(cudnn, input_descriptor, kernel_descriptor, conv_descriptor,
          output_descriptor, conv_algo, workspace_bytes))
        val sz = workspace_bytes / 1048576.0
        printf("Workspace size: %f MB", sz)

        // allocate memory for workspace 
        // don't need to `* sizeof(float)` because `workspace_bytes` already has that
        var d_workspace = cudaMalloc3[Float](workspace_bytes)
        
        // allocate memory for input image and copy the image to device
        var d_input = cudaMalloc2[Float](image_bytes)
        cudaCall(cudaMemcpyOfT[Float](d_input, img, image_bytes, host2device));

        // allocate memory for output image
        var d_output = cudaMalloc2[Float](image_bytes)
        cudaCall(cudaMemset2[Float](d_output, 0, image_bytes))
        
        // allocate memory for kernel and copy the kernel to device
        val d_kernel = cudaMalloc2[Float](3*3)
        cudaCall(cudaMemcpyOfT[Float](d_kernel, h_kernel, 3*3*3, host2device));

        // The convolution
        var alpha = 1.0f
        var beta = 0.0f
        // val alpha = NewArray[Int](1)
        //  alpha(0) = 1
        // val beta = NewArray[Int](1)
        // beta(0) = 0
        cudnnCheck(cudnnConvolutionForward(cudnn, alpha, input_descriptor, d_input, kernel_descriptor, d_kernel,
          conv_descriptor, conv_algo, d_workspace, workspace_bytes, beta, output_descriptor, d_output))

        
        // sigmoid
        /*
        val activation_descriptor = cudnnActivationDescriptor
        cudnnCheck(cudnnCreateActivationDescriptor(activation_descriptor))
        cudnnCheck(cudnnSetActivationDescriptor(activation_descriptor, cudnnActivationSigmoid, cudnnPropagateNan))
        */
        
        // copy the result image back to the host and print the output image
        val h_output = NewArray[Float](image_bytes)
        cudaCall(cudaMemcpyOfT(h_output, d_output, image_bytes, device2host))
        for (i <- (0 until image_bytes): Rep[Range]) {
          printf("%d, ", h_output(i))
        }

        // free resources
        cudaCall(cudaFree(d_kernel))
        cudaCall(cudaFree(d_input))
        cudaCall(cudaFree(d_output))
        cudaCall(cudaFree(d_workspace))

        cudnnCheck(cudnnDestroyTensorDescriptor(input_descriptor))
        cudnnCheck(cudnnDestroyTensorDescriptor(output_descriptor))
        cudnnCheck(cudnnDestroyFilterDescriptor(kernel_descriptor))
        cudnnCheck(cudnnDestroyConvolutionDescriptor(conv_descriptor))

        cudnnCheck(cudnnDestroy(cudnn))
      }
    }
    System.out.println(indent(driver.code))
  }
}