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

  test("one-conv-layer") {
    val driver = new DslDriverCCudnn[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val cudnn = cudnnHandle
        cudnnCheck(cudnnCreate(cudnn))

        // describe the input tensor
        // 1 batch, 3 channels, image shape 100x100
        val input_descriptor = cudnnTensorDescriptor
        cudnnCheck(cudnnCreateTensorDescriptor(input_descriptor))
        cudnnCheck(cudnnSetTensor4dDescriptor(input_descriptor, cudnnNHWC, cudnnFloat, 1, 3, 100, 100))

        // describe the output tensor
        // 1 batch, 3 channels, image shape 100x100
        val output_descriptor = cudnnTensorDescriptor
        cudnnCheck(cudnnCreateTensorDescriptor(output_descriptor))
        cudnnCheck(cudnnSetTensor4dDescriptor(output_descriptor, cudnnNHWC, cudnnFloat, 1, 3, 100, 100))

        // describe the kernel tensor
        // three in_channels, three out_channels, kernel shape 3x3
        val kernel_descriptor = cudnnFilterDescriptor
        cudnnCheck(cudnnCreateFilterDescriptor(kernel_descriptor))
        cudnnCheck(cudnnSetFilter4dDescriptor(kernel_descriptor, cudnnNCHW, cudnnFloat, 3, 3, 3, 3))

        // describe the convolution kernel
        val conv_descriptor = cudnnConvolutionDescriptor
        cudnnCheck(cudnnCreateConvolutionDescriptor(conv_descriptor))
        cudnnCheck(cudnnSetConvolution2dDescriptor(conv_descriptor, 1, 1, 1, 1, 1, 1, cudnnCrossCorrelation, cudnnFloat))

        // describe the convolution algorithm
        var res_count = 0
        var res = cudnnConvolutionFwdAlgoPerf
        cudnnCheck(cudnnFindConvolutionForwardAlgorithm(cudnn, input_descriptor, kernel_descriptor, conv_descriptor,
          output_descriptor, 1, res_count, res))
        val conv_algo = readField(res, "algo")

        // ask CuDNN for memory usage
        var workspace_bytes = 0
        cudnnCheck(cudnnGetConvolutionForwardWorkspaceSize(cudnn, input_descriptor, kernel_descriptor, conv_descriptor,
          output_descriptor, conv_algo, workspace_bytes))
        val sz = workspace_bytes / 1048576.0
        printf("Workspace size: %f MB", sz)

        // allocating memory
        var d_workspace = cudaMalloc2[Float](workspace_bytes)
        val image_bytes = 1 * 3 * 100 * 100 // batch_size * channels * height * width
        var d_input = cudaMalloc2[Float](image_bytes)
        // cudaMemcpy(d_input, image.ptr<float>(0), image_bytes, cudaMemcpyHostToDevice);

        var d_output = cudaMalloc2[Float](image_bytes)
        cudaCall(cudaMemset2[Float](d_output, 0, image_bytes))

        // kernel
        val h_kernel = NewArray[Float](3*3*3)
        for (i <- (0 until 3): Rep[Range]) {
          val idx = i * 9
          h_kernel(idx+0) = 1
          h_kernel(idx+1) = 1
          h_kernel(idx+2) = 1

          h_kernel(idx+3) = 1
          h_kernel(idx+4) = -7
          h_kernel(idx+5) = 1

          h_kernel(idx+6) = 1
          h_kernel(idx+7) = 1
          h_kernel(idx+8) = 1
        }
        
        // change size later
        val d_kernel = cudaMalloc2[Float](3*3*3)
        cudaCall(cudaMemcpy(d_kernel, h_kernel, SizeT(3*3*3), host2device));

        // The convolution
        // var alpha = 1
        // var beta = 0
        val alpha = NewArray[Int](1)
        alpha(0) = 1
        val beta = NewArray[Int](0)
        beta(0) = 0
        cudnnCheck(cudnnConvolutionForward(cudnn, alpha, input_descriptor, d_input, kernel_descriptor, d_kernel,
          conv_descriptor, conv_algo, d_workspace, workspace_bytes, beta, output_descriptor, d_output))
        
        // copy the result image back to the host

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