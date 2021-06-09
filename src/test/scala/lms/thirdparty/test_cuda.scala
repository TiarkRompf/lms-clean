package lms
package thirdparty.array_computation

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
import lms.collection.mutable.ArrayTypeLess
import lms.thirdparty.{ScannerOps, CCodeGenScannerOps}


class CudaTest extends TutorialFunSuite {
  val under = "thirdparty/cuda/"

  // first: get a driver :)
  abstract class DslDriverCCuda[A: Manifest, B: Manifest] extends DslDriverC[A,B] with CudaLibs { q =>
    override val codegen = new DslGenC with CCodeGenCudaOps {
      val IR: q.type = q
    }
    override val compilerCommand = "nvcc -std=c++11 -O3"

    val curPath = System.getProperty("user.dir")
    override val sourceFile = s"$curPath/snippet.cu"
    override val executable = s"$curPath/snippet"
  }

  abstract class DslDriverCCudaScan[A: Manifest, B: Manifest] extends DslDriverCCuda[A, B] with ScannerOps { q =>
    override val codegen = new DslGenC with CCodeGenCudaOps with CCodeGenScannerOps {
      val IR: q.type = q
    }
  }

  test("malloc_cuda_function") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val arr = Array(1,2,3,4,5)
        val cuda_arr = cudaMalloc2[Int](5)
        cudaCall(cudaMemcpyOfT(cuda_arr, arr, 5, host2device))
        val arr2 = NewArray[Int](5)
        cudaCall(cudaMemcpyOfT(arr2, cuda_arr, 5, device2host))
        printf("%d %d", arr2(0), arr2(4))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("malloc", driver.code, "cu")
  }

  test("remove_conditional") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        // now lets use the function with 2 different setting
        val arr = Array(1, 2, 3, 4, 5)
        val cuda_arr = cudaMalloc2[Int](5)
        cudaCall(cudaMemcpyOfT(cuda_arr, arr, 5, host2device))
        val cuda_grad = cudaMalloc2[Int](5)
        cudaArrayFill[Int](cuda_grad, 8, 5)

        // not inPlace
        val cuda_inG = cudaMalloc2[Int](5)
        val hardTanhGradIntFalse = hardTanhGrad[Int](false)
        hardTanhGradIntFalse(cuda_arr, cuda_inG, cuda_grad, -2, 2, 5, dim3(28), dim3(512))
        printf("%d %d %d %d %d", cuda_inG(0), cuda_inG(1), cuda_inG(2), cuda_inG(3), cuda_inG(4))

        // inPlace
        val hardTanhGradIntTrue = hardTanhGrad[Int](true)
        hardTanhGradIntTrue(cuda_arr, cuda_grad, cuda_grad, -2, 2, 5, dim3(28), dim3(512))
        printf("%d %d %d %d %d", cuda_grad(0), cuda_grad(1), cuda_grad(2), cuda_grad(3), cuda_grad(4))

        cudaCall(cudaFree(cuda_arr))
        cudaCall(cudaFree(cuda_grad))
        cudaCall(cudaFree(cuda_inG))
      }
    }
    check("remove_conditional", driver.code, "cu")
  }

  test("cuda_performance") {
    // Based on example shown in:
    // https://developer.nvidia.com/blog/how-implement-performance-metrics-cuda-cc/
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {

        val n = 4096
        val x = NewArray[Float](n)
        val y = NewArray[Float](n)
        val d_x = cudaMalloc2[Float](n)
        val d_y = cudaMalloc2[Float](n)

        for (i <- (0 until n): Rep[Range]) {
          x(i) = 1.0f
          y(i) = 2.0f
        }

        cudaCall(cudaMemcpyOfT[Float](d_x, x, n, host2device))
        cudaCall(cudaMemcpyOfT[Float](d_y, y, n, host2device))

        val time = measurement_cuda {
          val saxpyFloat = saxpy[Float]
          saxpyFloat(n, 2.0f, d_x, d_y, dim3((n + 511)/512), dim3(512))
        }

        cudaCall(cudaMemcpyOfT[Float](y, d_y, n, device2host))

        var maxError = 0.0f
        for (i <- (0 until n): Rep[Range]) {
          val error = Math.abs(y(i) - 4.0f)
          if (error > maxError) {
            maxError = error
          }
        }

        printf("Max error: %f\n", maxError)
        printf("Time: %f\n", time)
        printf("Effective Bandwidth (GB/s): %f\n", n*4*3/time/1e6);
      }
    }
    check("cuda_performance", driver.code, "cu")
  }

  test("kernel_2d_array") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        generate_comment("sanity check only, not runnable code")
        val a = NewSharedArray[Int](1)
        a(0) = 0
        val b = NewSharedArray[Int](2, 2)
        b(0, 0) = arg
        printf("%d", b(0, 1))
        printf("%d", b(1, 1))
        val c = NewSharedArray[Int](2, 2, 3)
        c(0, 0, 0) = arg
        printf("%d", c(0, 0, 0))
        printf("%d", c(0, 0, 1))
        printf("%d", c(0, 0, 2))
        printf("%d", c(0, 1, 0))
        printf("%d", c(0, 1, 1))
        printf("%d", c(0, 1, 2))
        printf("%d", c(1, 0, 0))
        printf("%d", c(1, 0, 1))
        printf("%d", c(1, 0, 2))
        printf("%d", c(1, 1, 0))
        printf("%d", c(1, 1, 1))
        printf("%d", c(1, 1, 2))
      }
    }
    check("kernel_2d_array", driver.code, "cu")
  }

  test("kernel_shared_array") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        val staticReverse = cudaGlobalFun[Array[Int], Int, Unit]((d, n) => {
          val s = NewSharedArray[Int](64)
          val t = threadIdxX
          val tr = n - t - 1
          s(t) = d(t)
          cudaSyncThreads
          d(t) = s(tr)
        })

        val dynamicReverse = cudaGlobalDynamicFun[Array[Int], Int, Unit]((d, n) => {
          val s = NewDynSharedArray[Int]
          val t = threadIdxX
          val tr = n - t - 1
          s(t) = d(t)
          cudaSyncThreads
          d(t) = s(tr)
        })

        val n = 64
        val a = NewArray[Int](n)
        val r = NewArray[Int](n)
        val d = NewArray[Int](n)

        for (i <- (0 until n): Rep[Range]) {
          a(i) = i
          r(i) = n - i - 1
          d(i) = 0
        }

        val d_d = cudaMalloc2[Int](n)

        cudaMemcpyOfT[Int](d_d, a, n, host2device)
        staticReverse(d_d, n, dim3(1), dim3(n))
        cudaMemcpyOfT[Int](d, d_d, n, device2host)

        for (i <- (0 until n): Rep[Range]) {
          if (d(i) != r(i)) {
            printf("Error!")
          }
        }

        cudaMemcpyOfT[Int](d_d, a, n, host2device)
        dynamicReverse(d_d, n, dim3(1), dim3(n), n * sizeOf[Int])
        cudaMemcpyOfT[Int](d, d_d, n, device2host)

        for (i <- (0 until n): Rep[Range]) {
          if (d(i) != r(i)) {
            printf("Error!")
          }
        }
      }
    }
    check("kernel_shared_array", driver.code, "cu")
  }

  test("fill_manual_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val cuda_arr = cudaMalloc2[Int](5)
        cudaArrayFill(cuda_arr, 8, 5)
        val arr = NewArray[Int](5)
        cudaCall(cudaMemcpyOfT(arr, cuda_arr, 5, device2host))
        printf("%d %d", arr(1), arr(4))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("fill", driver.code, "cu")
  }

  test("cap_manual_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val arr = Array(1, 2, 3, 4, 5)
        val cuda_arr = cudaMalloc2[Int](5)
        cudaCall(cudaMemcpyOfT(cuda_arr, arr, 5, host2device))
        cudaArrayClipAt(cuda_arr, 2, 5)
        val res = NewArray[Int](5)
        cudaCall(cudaMemcpyOfT(res, cuda_arr, 5, device2host))
        printf("%d, %d", res(0), res(4))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("cap", driver.code, "cu")
  }

  test("fill_gen_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        // use the CudaFill function in cuda.scala

        // now let's use the fill function
        val cuda_arr = cudaMalloc2[Float](5)
        val cudaFillInt = cudaFill[Float]
        cudaFillInt(cuda_arr, 3, 5, dim3(gridSize), dim3(blockSize))
        val arr = NewArray[Float](5)
        cudaCall(cudaMemcpyOfT(arr, cuda_arr, 5, device2host))
        printf("%f %f", arr(2), arr(3))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("cuda_global_fill", driver.code, "cu")
  }

  test("fill_gen_kernel2") {
    val driver = new DslDriverCCuda[Int, Unit] {
      import BaseTypeLess._
      import PrimitiveTypeLess._
      import ArrayTypeLess._
      import CUDATypeLess._

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val cuda_arr = CUDA_MALLOC(5, manifest[Float])
        val cudaFillFloat = CUDA_FILL_KERNEL(manifest[Float])
        cudaFillFloat(cuda_arr, 3.0f, 5, DIM3(gridSize), DIM3(blockSize))

        val arr = ARRAY(5, manifest[Float])
        CUDA_MEMCPY(arr, cuda_arr, 5, DEVICE2HOST, manifest[Float])
        printf("%f %f", Wrap[Float](arr(2).x), Wrap[Float](arr(3).x))
        CUDA_FREE(cuda_arr)
        unit(())
      }
    }
    check("cuda_global_fill2", driver.code, "cu")
  }

  test("cap_gen_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        // now let's use the cap function
        val arr = Array(1, 2, 3, 4, 5)
        val cuda_arr = cudaMalloc2[Int](5)
        cudaCall(cudaMemcpyOfT(cuda_arr, arr, 5, host2device))
        val capInt = cudaCap[Int]
        capInt(cuda_arr, 2, 5, dim3(28), dim3(512))
        val res = NewArray[Int](5)
        cudaCall(cudaMemcpyOfT(res, cuda_arr, 5, device2host))
        printf("%d, %d", res(0), res(4))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("cap_gen_kernel", driver.code, "cu")
  }

  test("embedding_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val n_embeddings = 20
        val embed_size = 60

        val embedding = NewArray[Float](n_embeddings * embed_size)
        scanFile[Float]("golden/embedding/embedding.data", embedding, n_embeddings * embed_size)
        val cuda_embedding = cudaMalloc2[Float](n_embeddings * embed_size)
        cudaCall(cudaMemcpyOfT(cuda_embedding, embedding, n_embeddings * embed_size, host2device))

        val n_indices = 10
        val indices = NewArray[Int](n_indices)
        scanFile[Int]("golden/embedding/indices.data", indices, n_indices)
        val cuda_indices = cudaMalloc2[Int](n_indices)
        cudaCall(cudaMemcpyOfT(cuda_indices, indices, n_indices, host2device))

        val output = NewArray[Float](n_indices * embed_size)
        val cuda_output = cudaMalloc2[Float](n_indices * embed_size)

        val cudaEmbeddingKernel = cudaEmbedding[Float]
        cudaEmbeddingKernel(cuda_embedding, cuda_indices, cuda_output, embed_size, dim3(embed_size), dim3(n_indices))
        cudaCall(cudaMemcpyOfT(output, cuda_output, n_indices * embed_size, device2host))
        checkFile[Float]("golden/embedding/output.data", output, n_indices * embed_size)
      }
    }
    check("embedding", driver.code, "cu")
  }

  test("embeddin_grad_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val n_embeddings = 20
        val embed_size = 60
        val n_indices = 10
        val paddingIdx = -1
        val gridSize = (embed_size + 31) / 32

        val indices = NewArray[Int](n_indices)
        scanFile[Int]("golden/embedding/indices.data", indices, n_indices)
        val cuda_indices = cudaMalloc2[Int](n_indices)
        cudaCall(cudaMemcpyOfT(cuda_indices, indices, n_indices, host2device))

        val outputGrad = NewArray[Float](n_indices * embed_size)
        scanFile[Float]("golden/embedding/output_grad.data", outputGrad, n_indices * embed_size)
        val cuda_output_grad = cudaMalloc2[Float](n_indices * embed_size)
        cudaCall(cudaMemcpyOfT(cuda_output_grad, outputGrad, n_indices * embed_size, host2device))

        val embeddingGrad = NewArray[Float](n_embeddings * embed_size)
        val cuda_embedding_grad = cudaMalloc2[Float](n_embeddings * embed_size)

        val cudaEmbeddingGradKernel = cudaEmbeddingGrad[Float]
        cudaEmbeddingGradKernel(cuda_indices, cuda_output_grad, cuda_embedding_grad, n_indices, embed_size, paddingIdx, dim3(gridSize), dim3(32, 32), 32 * 32 * sizeOf[Int] + 32 * 32 * sizeOf[Float])

        cudaCall(cudaMemcpyOfT(embeddingGrad, cuda_embedding_grad, n_embeddings * embed_size, device2host))
        checkFile[Float]("golden/embedding/embedding_grad.data", embeddingGrad, n_embeddings * embed_size)
      }
    }    

    check("embedding_grad", driver.code, "cu")
  }

  test("softmax_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        // dim0 and dim1 picked arbitrary
        val dim0 = 20
        val dim1 = 782
        val size = dim0 * dim1

        // inputs to the test
        val input = NewArray[Float](dim0 * dim1)
        scanFile[Float]("golden/softmax/input.data", input, size)
        val outputGrad = NewArray[Float](size)
        scanFile[Float]("golden/softmax/output_grad.data", outputGrad, size)

        val cudaInput = cudaMalloc2[Float](size)
        val cudaOutputGrad = cudaMalloc2[Float](size)
        cudaCall(cudaMemcpyOfT[Float](cudaInput, input, size, host2device))
        cudaCall(cudaMemcpyOfT[Float](cudaOutputGrad, outputGrad, size, host2device))

        val inputGrad = NewArray[Float](size)
        val output = NewArray[Float](size)
        val cudaInputGrad = cudaMalloc2[Float](size)
        val cudaOutput = cudaMalloc2[Float](size)

        // forward
        val softmaxKernel = cudaSoftmax[Float](false)
        softmaxKernel(cudaInput, cudaOutput, dim1, dim3(dim0, 1, 1), dim3(1024, 1, 1), 1024 * 4)

        // backward
        val softmaxGradKernel = cudaSoftmaxGrad[Float](false)
        softmaxGradKernel(cudaInputGrad, cudaOutputGrad, cudaOutput, dim1, dim3(dim0, 1, 1), dim3(1024, 1, 1), 1024 * 4)

        // move outputs to the host
        cudaCall(cudaMemcpyOfT[Float](inputGrad, cudaInputGrad, size, device2host))
        cudaCall(cudaMemcpyOfT[Float](output, cudaOutput, size, device2host))

        // check with expected output
        checkFile[Float]("golden/softmax/input_grad.data", inputGrad, size)
        checkFile[Float]("golden/softmax/output.data", output, size)
      }
    }
    check("softmax", driver.code, "cu")
  }

  test("logSoftmax_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        // dim0 and dim1 picked arbitrary
        val dim0 = 32
        val dim1 = 533
        val size = dim0 * dim1

        // inputs to the test
        val input = NewArray[Float](dim0 * dim1)
        scanFile[Float]("golden/logSoftmax/input.data", input, size)
        val outputGrad = NewArray[Float](size)
        scanFile[Float]("golden/logSoftmax/output_grad.data", outputGrad, size)

        val cudaInput = cudaMalloc2[Float](size)
        val cudaOutputGrad = cudaMalloc2[Float](size)
        cudaCall(cudaMemcpyOfT[Float](cudaInput, input, size, host2device))
        cudaCall(cudaMemcpyOfT[Float](cudaOutputGrad, outputGrad, size, host2device))

        val inputGrad = NewArray[Float](size)
        val output = NewArray[Float](size)
        val cudaInputGrad = cudaMalloc2[Float](size)
        val cudaOutput = cudaMalloc2[Float](size)

        // forward
        val softmaxKernel = cudaSoftmax[Float](true)
        softmaxKernel(cudaInput, cudaOutput, dim1, dim3(dim0, 1, 1), dim3(1024, 1, 1), 1024 * 4)

        // backward
        val softmaxGradKernel = cudaSoftmaxGrad[Float](true)
        softmaxGradKernel(cudaInputGrad, cudaOutputGrad, cudaOutput, dim1, dim3(dim0, 1, 1), dim3(1024, 1, 1), 1024 * 4)

        // move outputs to the host
        cudaCall(cudaMemcpyOfT[Float](inputGrad, cudaInputGrad, size, device2host))
        cudaCall(cudaMemcpyOfT[Float](output, cudaOutput, size, device2host))

        // check with expected output
        checkFile[Float]("golden/logSoftmax/input_grad.data", inputGrad, size)
        checkFile[Float]("golden/logSoftmax/output.data", output, size)
      }
    }
    check("logSoftmax", driver.code, "cu")
  }

  test("maskedFill_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val d0 = 8
        val d1 = 8
        val n = d0 * d1
        val maskValue = 0.0f

        val input = NewArray[Float](n)
        scanFile[Float]("golden/maskedFill/input.data", input, n)
        val cuda_input = cudaMalloc2[Float](n)
        cudaCall(cudaMemcpyOfT[Float](cuda_input, input, n, host2device))

        val mask = NewArray[Int](n)
        scanFile[Int]("golden/maskedFill/mask.data", mask, n)
        val cuda_mask = cudaMalloc2[Int](n)
        cudaCall(cudaMemcpyOfT[Int](cuda_mask, mask, n, host2device))

        val output = NewArray[Float](n)
        val cuda_output = cudaMalloc2[Float](n)
        val maskedFillKernel = cudaMaskedFill[Float](false)
        maskedFillKernel(cuda_input, cuda_output, cuda_mask, maskValue,
          d0,   /* dim0_shape */
          d1,   /* dim1_shape */
          d0,   /* dim0_stride */
          1,    /* dim1_stride */
          n, dim3((n + 511)/512), dim3(512))
        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, n, device2host))
        checkFile[Float]("golden/maskedFill/output.data", output, n)

        val cuda_doutput = cudaMalloc2[Float](n)
        val cudaFillKernel = cudaFill[Float]
        cudaFillKernel(cuda_doutput, 1.0f, n, dim3((n + 511)/512), dim3(512))

        val cuda_dinput = cudaMalloc2[Float](n)
        val maskedFillGradKernel = cudaMaskedFillGrad[Float](false)
        maskedFillGradKernel(cuda_doutput, cuda_dinput, cuda_mask,
          d0,   /* dim0_shape */
          d1,   /* dim1_shape */
          d0,   /* dim0_stride */
          1,    /* dim1_stride */
          n, dim3((n + 511)/512), dim3(512))

        val dinput =  NewArray[Float](n)
        cudaCall(cudaMemcpyOfT[Float](dinput, cuda_dinput, n, device2host))
        checkFile[Float]("golden/maskedFill/input_grad.data", dinput, n)
      }
    }
    check("maskedFill", driver.code, "cu")
  }

  test("transpose_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val rcount = 213
        val ccount = 56
        val size = rcount * ccount
        val tileDim = 32
        val blockRows = 8
        val dimGrid = dim3((ccount + tileDim - 1) / tileDim, (rcount + tileDim - 1) / tileDim)
        val dimBlock = dim3(tileDim, blockRows)

        val input = NewArray[Int](size)
        val output = NewArray[Int](size)
        val golden = NewArray[Int](size)

        for (i <- (0 until size): Rep[Range]) {
          input(i) = i
        }

        for (j <- (0 until ccount): Rep[Range]) {
          for (i <- (0 until rcount): Rep[Range]) {
            golden(j * rcount + i) = input(i * ccount + j)
          }
        }

        val cuda_input = cudaMalloc2[Int](size)
        val cuda_output = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT[Int](cuda_input, input, size, host2device))

        val transposeKernel = cudaTranspose[Int]
        transposeKernel(cuda_input, cuda_output, rcount, ccount, dimGrid, dimBlock)
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        for (i <- (0 until size): Rep[Range]) {
          if (golden(i) != output(i)) {
            printf("Transpose Incorrect!\n")
            exit(1)
          }
        }
        printf("Transpose Correct\n")
      }
    }
    check("transpose", driver.code, "cu")
  }

  test("permute_kernel_10") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val rcount = 200
        val ccount = 100
        val size = rcount * ccount
        val tileDim = 32
        val blockRows = 8
        val input = NewArray[Int](size)
        val output = NewArray[Int](size)

        for (i <- (0 until size): Rep[Range]) {
          input(i) = i + 1
        }

        val cuda_input = cudaMalloc2[Int](size)
        val cuda_output = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT[Int](cuda_input, input, size, host2device))

        val transposeKernel = cudaTranspose2[Int]
        transposeKernel(cuda_input, cuda_output, ccount, rcount,
          dim3((rcount+tileDim-1)/tileDim, (ccount+tileDim-1)/tileDim), dim3(tileDim, blockRows))
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        for (i <- (0 until rcount): Rep[Range]) {
          for (j <- (0 until ccount): Rep[Range]) {
            if (input(rcount * j + i) != output(ccount * i + j)) {
              printf("Transpose Incorrect!\n")
              exit(1)
            }
          }
        }
        printf("Transpose Correct\n")
      }
    }
    check("permute_10", driver.code, "cu")
  }

  test("permute3D") {
    val driver = new DslDriverCCudaScan[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val dimX = 50
        val dimY = 55
        val dimZ = 60
        val size = dimX * dimY * dimZ
        val tileDim = 32
        val blockRows = 8

        // test 021
        val input021 = NewArray[Int](size)
        scanFile[Int]("golden/permute3D_021/input.data", input021, size)
        val cuda_input021 = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT(cuda_input021, input021, size, host2device))

        val permuteKernel021 = cudaPermute3D[Int](List(0,2,1))
        val block021 = dim3(tileDim, blockRows, 1)
        val grid021 = dim3((dimX+tileDim-1)/tileDim, (dimY+tileDim-1)/tileDim, dimZ)

        val cuda_output021 = cudaMalloc2[Int](size)
        permuteKernel021(cuda_input021, cuda_output021, dimZ, dimY, dimX, grid021, block021)

        val output021 = NewArray[Int](size)
        cudaCall(cudaMemcpyOfT(output021, cuda_output021, size, device2host))
        checkFile[Int]("golden/permute3D_021/output.data", output021, size)

        // test 120
        val input120 = NewArray[Int](size)
        scanFile[Int]("golden/permute3D_120/input.data", input120, size)
        val cuda_input120 = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT(cuda_input120, input120, size, host2device))

        val permuteKernel120 = cudaPermute3D[Int](List(1,2,0))
        val block120 = dim3(tileDim, 1, blockRows)
        val grid120 = dim3((dimX+tileDim-1)/tileDim, dimY, (dimZ+tileDim-1)/tileDim)

        val cuda_output120 = cudaMalloc2[Int](size)
        permuteKernel120(cuda_input120, cuda_output120, dimZ, dimY, dimX, grid120, block120)

        val output120 = NewArray[Int](size)
        cudaCall(cudaMemcpyOfT(output120, cuda_output120, size, device2host))
        checkFile[Int]("golden/permute3D_120/output.data", output120, size)

        // test 210
        val input210 = NewArray[Int](size)
        scanFile[Int]("golden/permute3D_210/input.data", input210, size)
        val cuda_input210 = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT(cuda_input210, input210, size, host2device))

        val permuteKernel210 = cudaPermute3D[Int](List(2,1,0))
        val block210 = dim3(tileDim, 1, blockRows)
        val grid210 = dim3((dimX+tileDim-1)/tileDim, dimY, (dimZ+tileDim-1)/tileDim)

        val cuda_output210 = cudaMalloc2[Int](size)
        permuteKernel210(cuda_input210, cuda_output210, dimZ, dimY, dimX, grid210, block210)

        val output210 = NewArray[Int](size)
        cudaCall(cudaMemcpyOfT(output210, cuda_output210, size, device2host))
        checkFile[Int]("golden/permute3D_210/output.data", output210, size)

        // test 201
        val input201 = NewArray[Int](size)
        scanFile[Int]("golden/permute3D_201/input.data", input201, size)
        val cuda_input201 = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT(cuda_input201, input201, size, host2device))

        val permuteKernel201 = cudaPermute3D[Int](List(2,0,1))
        val block201 = dim3(tileDim, blockRows, 1)
        val grid201 = dim3((dimX+tileDim-1)/tileDim, (dimY+tileDim-1)/tileDim, dimZ)

        val cuda_output201 = cudaMalloc2[Int](size)
        permuteKernel201(cuda_input201, cuda_output201, dimZ, dimY, dimX, grid201, block201)

        val output201 = NewArray[Int](size)
        cudaCall(cudaMemcpyOfT(output201, cuda_output201, size, device2host))
        checkFile[Int]("golden/permute3D_201/output.data", output201, size)
      }
    }
    check("permute3D", driver.code, "cu")
  }

  test("permute_kernel_102_big") {
    val driver = new DslDriverCCudaScan[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val dimX = 600
        val blockDimX = 100
        val dimY = 60
        val dimZ = 40
        val size = dimZ * dimY * dimX

        val input = NewArray[Int](size)
        scanFile[Int]("golden/permute_kernel_102_big/input.data", input, size)
        val cuda_input = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT(cuda_input, input, size, host2device))

        val output = NewArray[Int](size)
        val cuda_output = cudaMalloc2[Int](size)

        val permuteKernel = cudaPermute102[Int]
        permuteKernel(cuda_input, cuda_output, dimZ, dimY, dimX, dim3(dimY, dimZ, 1), dim3(blockDimX, 1, 1))
        cudaCall(cudaMemcpyOfT(output, cuda_output, size, device2host))
        checkFile[Int]("golden/permute_kernel_102_big/output.data", output, size)
      }
    }
    check("permute_102_big", driver.code, "cu")
  }

  test("transpose_naive_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val rcount = 64
        val ccount = 64
        val size = rcount * ccount
        val tileDim = 32
        val blockRows = 8
        val input = NewArray[Int](size)
        val output = NewArray[Int](size)

        for (i <- (0 until size): Rep[Range]) {
          input(i) = i + 1
        }

        val cuda_input = cudaMalloc2[Int](size)
        val cuda_output = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT[Int](cuda_input, input, size, host2device))

        val transposeKernel = cudaTransposeNaive[Int]
        transposeKernel(cuda_input, cuda_output, dim3(rcount/tileDim, ccount/tileDim), dim3(tileDim, blockRows))
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        for (i <- (0 until rcount): Rep[Range]) {
          for (j <- (0 until ccount): Rep[Range]) {
            if (input(rcount * i + j) != output(rcount * j + i)) {
              printf("Transpose Incorrect!\n")
              exit(1)
            }
          }
        }
        printf("Transpose Correct\n")
      }
    }
    check("transpose_naive", driver.code, "cu")
  }

  test("copy_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val rcount = 64
        val ccount = 64
        val size = rcount * ccount
        val tileDim = 32
        val blockRows = 8
        val input = NewArray[Int](size)
        val output = NewArray[Int](size)

        for (i <- (0 until size): Rep[Range]) {
          input(i) = i + 1
        }

        val cuda_input = cudaMalloc2[Int](size)
        val cuda_output = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT[Int](cuda_input, input, size, host2device))

        val copyKernel = cudaMatrixCopy[Int]
        copyKernel(cuda_input, cuda_output, dim3(rcount/tileDim, ccount/tileDim), dim3(tileDim, blockRows))
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        for (i <- (0 until rcount): Rep[Range]) {
          for (j <- (0 until ccount): Rep[Range]) {
            if (input(rcount * i + j) != output(rcount * i + j)) {
              printf("Copy Incorrect!\n")
              exit(1)
            }
          }
        }
        printf("Copy Correct\n")
      }
    }
    check("copy", driver.code, "cu")
  }

  test("transpose_performance") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val rcount = 64
        val ccount = 64
        val size = rcount * ccount
        val tileDim = 32
        val blockRows = 8
        val input = NewArray[Int](size)
        val output = NewArray[Int](size)
        val iter_count = 100

        for (i <- (0 until size): Rep[Range]) {
          input(i) = i + 1
        }

        val cuda_input = cudaMalloc2[Int](size)
        val cuda_output = cudaMalloc2[Int](size)
        cudaCall(cudaMemcpyOfT[Int](cuda_input, input, size, host2device))

        val copyTime = measurement_cuda {
          val copyKernel = cudaMatrixCopy[Int]
          for (i <- (0 until iter_count): Rep[Range]) {
            copyKernel(cuda_input, cuda_output, dim3(rcount/tileDim, ccount/tileDim), dim3(tileDim, blockRows))
          }
        }
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        val naive_transpose_time = measurement_cuda {
          val naiveTransposeKernel = cudaTransposeNaive[Int]
          for (i <- (0 until iter_count): Rep[Range]) {
            naiveTransposeKernel(cuda_input, cuda_output, dim3(rcount/tileDim, ccount/tileDim), dim3(tileDim, blockRows))
          }
        }
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        val transpose_time = measurement_cuda {
          val transposeKernel = cudaTranspose[Int]
          for (i <- (0 until iter_count): Rep[Range]) {
            val dimGrid = dim3((ccount + tileDim - 1) / tileDim, (rcount + tileDim - 1) / tileDim)
            val dimBlock = dim3(tileDim, blockRows)
            transposeKernel(cuda_input, cuda_output, rcount, ccount, dimGrid, dimBlock)
          }
        }
        cudaCall(cudaMemcpyOfT[Int](output, cuda_output, size, device2host))

        printf("COPY KERNEL STATS:\n")
        printf("Time: %f ms\n", copyTime)
        printf("Bandwidth (GB/s): %f\n", 2 * size * 1e-6 / copyTime)
        printf("=======================\n")
        printf("NAIVE TRANSPOSE KERNEL STATS:\n")
        printf("Time: %f ms\n", naive_transpose_time)
        printf("Bandwidth (GB/s): %f\n", 2 * size * 1e-6 / naive_transpose_time)
        printf("=======================\n")
        printf("COALESCED TRANSPOSE KERNEL STATS:\n")
        printf("Time: %f ms\n", transpose_time)
        printf("Bandwidth (GB/s): %f\n", 2 * size * 1e-6 / transpose_time)
      }
    }
    check("transpose_performance", driver.code, "cu")
  }

  test("split2_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val dimZ = 1
        val dimY = 4
        val dimX = 4
        val dimXs = List(2, 2)

        val in_sz = dimZ * dimY * dimX
        val out0_sz = dimZ * dimY * dimXs(0)
        val out1_sz = dimZ * dimY * dimXs(1)

        val input = NewArray[Float](in_sz)
        scanFile[Float]("golden/split2/input.data", input, in_sz)
        val cuda_input = cudaMalloc2[Float](in_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input, input, in_sz, host2device))

        val output0 = NewArray[Float](out0_sz)
        val cuda_output0 = cudaMalloc2[Float](out0_sz)

        val output1 = NewArray[Float](out1_sz)
        val cuda_output1 = cudaMalloc2[Float](out1_sz)

        cuda3DSplitWrap[Float](cuda_input,
          List(cuda_output0, cuda_output1),
          dimZ, dimY, dimXs)

        cudaCall(cudaMemcpyOfT[Float](output0, cuda_output0, out0_sz, device2host))
        cudaCall(cudaMemcpyOfT[Float](output1, cuda_output1, out1_sz, device2host))

        generate_comment("check cuda3DSplit kernel of section 2 against individual outputs")
        checkFile[Float]("golden/split2/output0.data", output0, out0_sz)
        checkFile[Float]("golden/split2/output1.data", output1, out1_sz)
      }
    }
    check("split2", driver.code, "cu")
  }

  test("concat2_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val d0 = 3
        val d1 = 5
        val dimZ = 3
        val dimY = 2
        val d_other = dimZ * dimY

        val in0_sz = d0 * d_other
        val in1_sz = d1 * d_other
        val out_sz = (d0 + d1) * d_other

        val input0 = NewArray[Float](in0_sz)
        scanFile[Float]("golden/concat2/input0.data", input0, in0_sz)
        val cuda_input0 = cudaMalloc2[Float](in0_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input0, input0, in0_sz, host2device))

        val input1 = NewArray[Float](in1_sz)
        scanFile[Float]("golden/concat2/input1.data", input1, in1_sz)
        val cuda_input1 = cudaMalloc2[Float](in1_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input1, input1, in1_sz, host2device))

        val output = NewArray[Float](out_sz)
        val cuda_output = cudaMalloc2[Float](out_sz)

        cuda3DConcatWrap[Float](List(cuda_input0, cuda_input1),
          cuda_output,
          dimZ, dimY, List(d0, d1))

        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, out_sz, device2host))

        generate_comment("check general cuda3DConcat kernel")
        checkFile[Float]("golden/concat2/output.data", output, out_sz)
      }
    }
    check("concat2", driver.code, "cu")
  }

  test("split3_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {

        val d0 = 2
        val d1 = 2
        val d2 = 1
        val dimZ = 1
        val dimY = 4
        val d_other = dimZ * dimY

        val in_sz = d_other * (d0 + d1 + d2)
        val out0_sz = d_other * d0
        val out1_sz = d_other * d1
        val out2_sz = d_other * d2

        val input = NewArray[Float](in_sz)
        scanFile[Float]("golden/split3/input.data", input, in_sz)
        val cuda_input = cudaMalloc2[Float](in_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input, input, in_sz, host2device))

        val output0 = NewArray[Float](out0_sz)
        val cuda_output0 = cudaMalloc2[Float](out0_sz)

        val output1 = NewArray[Float](out1_sz)
        val cuda_output1 = cudaMalloc2[Float](out1_sz)

        val output2 = NewArray[Float](out2_sz)
        val cuda_output2 = cudaMalloc2[Float](out2_sz)

        cuda3DSplitWrap[Float](
          cuda_input, List(cuda_output0, cuda_output1, cuda_output2),
          dimZ, dimY, List(d0, d1, d2))

        cudaCall(cudaMemcpyOfT[Float](output0, cuda_output0, out0_sz, device2host))
        cudaCall(cudaMemcpyOfT[Float](output1, cuda_output1, out1_sz, device2host))
        cudaCall(cudaMemcpyOfT[Float](output2, cuda_output2, out2_sz, device2host))

        generate_comment("check cuda3DSplit kernel of section 3 against individual outputs")
        checkFile[Float]("golden/split3/output0.data", output0, out0_sz)
        checkFile[Float]("golden/split3/output1.data", output1, out1_sz)
        checkFile[Float]("golden/split3/output2.data", output2, out2_sz)
      }
    }
    check("split3", driver.code, "cu")
  }

  test("concat3_kernel") {
    val driver = new DslDriverCCudaScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val d0 = 3
        val d1 = 2
        val d2 = 1
        val dimZ = 3
        val dimY = 2
        val d_other = dimZ * dimY

        val in0_sz = d0 * d_other
        val in1_sz = d1 * d_other
        val in2_sz = d2 * d_other
        val out_sz = (d0 + d1) * d_other

        val input0 = NewArray[Float](in0_sz)
        scanFile[Float]("golden/concat3/input0.data", input0, in0_sz)
        val cuda_input0 = cudaMalloc2[Float](in0_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input0, input0, in0_sz, host2device))

        val input1 = NewArray[Float](in1_sz)
        scanFile[Float]("golden/concat3/input1.data", input1, in1_sz)
        val cuda_input1 = cudaMalloc2[Float](in1_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input1, input1, in1_sz, host2device))

        val input2 = NewArray[Float](in2_sz)
        scanFile[Float]("golden/concat3/input2.data", input2, in2_sz)
        val cuda_input2 = cudaMalloc2[Float](in2_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input2, input2, in2_sz, host2device))

        val output = NewArray[Float](out_sz)
        val cuda_output = cudaMalloc2[Float](out_sz)

        cuda3DConcatWrap[Float](List(cuda_input0, cuda_input1, cuda_input2),
          cuda_output,
          dimZ, dimY, List(d0, d1, d2))

        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, out_sz, device2host))

        checkFile[Float]("golden/concat3/output.data", output, out_sz)
      }
    }

    check("concat3", driver.code, "cu")
  }
}

