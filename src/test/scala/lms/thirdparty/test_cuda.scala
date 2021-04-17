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

  abstract class DslDriverCCudeScan[A: Manifest, B: Manifest] extends DslDriverCCuda[A, B] with ScannerOps { q =>
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
    val driver = new DslDriverCCudeScan[Int, Unit] {

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

  // TODO(Supun): Currently, this just emits the generated code (always passes the test)
  //  and we need to manually run the generated code to test
  test("softmax_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val dataSize = 2*2*3
        val data = Seq[Rep[Float]](1, 2, 3, 4, 6, 8, 1, 5, 9, 1, 9, 12)
        val hostInput = Array[Float](data:_*)
        val devInput = cudaMalloc2[Float](dataSize)

        val hostOutput = NewArray[Float](dataSize)
        val devOutput = cudaMalloc2[Float](dataSize)

        val expectedOutput =
          Array[Float](Seq[Rep[Float]](0.09f, 0.2447f, 0.6652f, 0.0158f, 0.1173f, 0.8668f,
                            0.0003f, 0.01798f, 0.9817f, 0.000015f, 0.0474f, 0.9525f):_*)

        cudaCall(cudaMemcpyOfT(devInput, hostInput, dataSize, host2device))
        val softmaxKernel = softmax[Float](false)
        softmaxKernel(devInput, devOutput, 3, dim3(2*2, 1, 1), dim3(1024, 1, 1), 1024 * 4)

        cudaCall(cudaMemcpyOfT(hostOutput, devOutput, dataSize, device2host))

        // validate the output
        for(i <- (0 until dataSize): Rep[Range]) {
          if (Math.abs(hostOutput(i) - expectedOutput(i)) > 0.0001f) {
            printf("Error! Expected: %.3f got %.3f\n", expectedOutput(i), hostOutput(i))
          } else {
            printf("Matched\n")
          }
        }
      }
    }
    check("softmax", driver.code, "cu")
  }

  // TODO(Supun): Remove. Added just to observe and manually test the kernel
  // Tested by replacing the generated kernel in cublas_header and running lantern(old) test
  test("softmaxGrad_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val dummy = NewArray[Float](1)

        val softmaxGradKernel = softmaxGrad[Float](false)
        softmaxGradKernel(dummy, dummy, dummy, 3, dim3(2*2, 1, 1), dim3(1024, 1, 1), 1024 * 4)
      }
    }
    // check("kernel_2d_array", driver.code, "cu")
  }

  test("maskedFill_kernel") {
    val driver = new DslDriverCCudeScan[Int, Unit] {
      
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
        maskedFillKernel(cuda_input, cuda_output, cuda_mask, maskValue, d0, d1, d0, 1, n, dim3((n + 511)/512), dim3(512))
        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, n, device2host))
        checkFile[Float]("golden/maskedFill/output.data", output, n)
      }
    }
    check("maskedFill", driver.code, "cu")
  }
  
  test("transpose_kernel") {
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

        val transposeKernel = cudaTranspose[Int]
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
    check("transpose", driver.code, "cu")
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
            transposeKernel(cuda_input, cuda_output, dim3(rcount/tileDim, ccount/tileDim), dim3(tileDim, blockRows))
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
    val driver = new DslDriverCCudeScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val d0 = 2
        val d1 = 2
        val d_other = 4

        val in_sz = d_other * (d0 + d1)
        val out0_sz = d_other * d0
        val out1_sz = d_other * d1

        val input = NewArray[Float](in_sz)
        scanFile[Float]("golden/split2/input.data", input, in_sz)
        val cuda_input = cudaMalloc2[Float](in_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input, input, in_sz, host2device))

        val output0 = NewArray[Float](out0_sz)
        val cuda_output0 = cudaMalloc2[Float](out0_sz)

        val output1 = NewArray[Float](out1_sz)
        val cuda_output1 = cudaMalloc2[Float](out1_sz)

        val splitKernel = cuda3DSplit2[Float]
        splitKernel(cuda_input, d_other, cuda_output0, d0, cuda_output1, d1, dim3((in_sz + 511)/512), dim3(512))

        cudaCall(cudaMemcpyOfT[Float](output0, cuda_output0, out0_sz, device2host))
        cudaCall(cudaMemcpyOfT[Float](output1, cuda_output1, out1_sz, device2host))

        checkFile[Float]("golden/split2/output0.data", output0, out0_sz)
        checkFile[Float]("golden/split2/output1.data", output1, out1_sz)
      }
    }
    check("split2", driver.code, "cu")
  }

  test("concat2_kernel") {
    val driver = new DslDriverCCudeScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val d0 = 3
        val d1 = 5
        val d_other = 3*2
        
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

        val concatKernel = cuda3DConcat2[Float]
        concatKernel(cuda_input0, d0, cuda_input1, d1, cuda_output, d_other, dim3((out_sz + 511)/512), dim3(512))

        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, out_sz, device2host))

        checkFile[Float]("golden/concat2/output.data", output, out_sz)
      }
    }
    check("concat2", driver.code, "cu")
  }

  // TODO(Luke): change to check against pytorch
  test("split3_kernel") {
    val driver = new DslDriverCCudeScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {

        val d0 = 2
        val d1 = 2
        val d2 = 1
        val d_other = 2*2

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

        val split3Kernel = cuda3DSplit3[Float]
        split3Kernel(cuda_input, d_other, cuda_output0, d0, cuda_output1, d1, cuda_output2, d2, dim3((in_sz + 511)/512), dim3(512))
        
        cudaCall(cudaMemcpyOfT[Float](output0, cuda_output0, out0_sz, device2host))
        cudaCall(cudaMemcpyOfT[Float](output1, cuda_output1, out1_sz, device2host))
        cudaCall(cudaMemcpyOfT[Float](output2, cuda_output2, out2_sz, device2host))

        // check individual outputs
        checkFile[Float]("golden/split3/output0.data", output0, out0_sz)
        checkFile[Float]("golden/split3/output1.data", output1, out1_sz)
        checkFile[Float]("golden/split3/output2.data", output2, out2_sz)
      }
    }
    check("split3", driver.code, "cu")
  }

  test("split_kernel") {
    val driver = new DslDriverCCudeScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {

        val d0 = 2
        val d1 = 2
        val d2 = 1
        val d_other = 2*2

        val in_sz = d_other * (d0 + d1 + d2)
        val out0_sz = d_other * d0
        val out1_sz = d_other * d1
        val out2_sz = d_other * d2

        val input = NewArray[Float](in_sz)
        scanFile[Float]("golden/split3/input.data", input, in_sz)
        val cuda_input = cudaMalloc2[Float](in_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input, input, in_sz, host2device))

        val output = NewArray[Float](in_sz)
        val cuda_output = cudaMalloc2[Float](in_sz)

        val splitKernel = cuda3DSplit[Float](3, List(2, 2, 1, 3))
        splitKernel(cuda_input, cuda_output, dim3((in_sz + 511)/512), dim3(512))
        
        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, in_sz, device2host))

        checkFile[Float]("golden/split3/output.data", output, in_sz)
      }
    }
    check("split3n", driver.code, "cu")
  }

  test("concat_kernel") {
    val driver = new DslDriverCCudeScan[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val d0 = 3
        val d1 = 2
        val d2 = 1
        val d_other = 3*2
        
        val in0_sz = d0 * d_other
        val in1_sz = d1 * d_other
        val in2_sz = d2 * d_other
        val out_sz = (d0 + d1) * d_other

        val input0 = NewArray[Float](in0_sz)
        // scanFile[Float]("golden/concat2/input0.data", input0, in0_sz)
        for (i <- (0 until in0_sz): Rep[Range]) {
          input0(i) = i
        }
        val cuda_input0 = cudaMalloc2[Float](in0_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input0, input0, in0_sz, host2device))

        val input1 = NewArray[Float](in1_sz)
        // scanFile[Float]("golden/concat2/input1.data", input1, in1_sz)
        for (i <- (0 until in1_sz): Rep[Range]) {
          input1(i) = i + 50
        }
        val cuda_input1 = cudaMalloc2[Float](in1_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input1, input1, in1_sz, host2device))

        val input2 = NewArray[Float](in2_sz)
        // scanFile[Float]("golden/concat2/input1.data", input1, in1_sz)
        for (i <- (0 until in2_sz): Rep[Range]) {
          input2(i) = i + 100
        }
        val cuda_input2 = cudaMalloc2[Float](in2_sz)
        cudaCall(cudaMemcpyOfT[Float](cuda_input2, input2, in2_sz, host2device))

        val output = NewArray[Float](out_sz)
        val cuda_output = cudaMalloc2[Float](out_sz)

        val concatKernel = cuda3DConcat[Float](3, List(d0, d1, d2, d_other), List(cuda_input0, cuda_input1, cuda_input2))
        concatKernel(cuda_output, dim3((out_sz + 511)/512), dim3(512))

        cudaCall(cudaMemcpyOfT[Float](output, cuda_output, out_sz, device2host))

        // checkFile[Float]("golden/concat2/output.data", output, out_sz)
        printf("output: [")
        for (i <- (0 until out_sz): Rep[Range]) {
          printf("%f,", output(i))
        }
        printf("]\n")

      }
    }
    System.out.println(indent(driver.code))
  }
}

