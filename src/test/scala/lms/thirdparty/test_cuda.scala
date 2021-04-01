package lms
package thirdparty.array_computation

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
import lms.collection.mutable.ArrayTypeLess


class CudaTest extends TutorialFunSuite {
  val under = "thirdparty/cuda/"

  // first: get a driver :)
  abstract class DslDriverCCuda[A: Manifest, B: Manifest] extends DslDriverC[A,B] with CudaOps { q =>
    override val codegen = new DslGenC with CCodeGenCudaOps with PrimitiveOps {
      val IR: q.type = q
    }
    override val compilerCommand = "nvcc -std=c++11 -O3"

    val curPath = System.getProperty("user.dir")
    override val sourceFile = s"$curPath/snippet.cu"
    override val executable = s"$curPath/snippet"
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
    System.out.println(indent(driver.code))
  }

  test("kernel_reverse") {
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
    check("kernel_reverse", driver.code, "cu")
  }

  test("kernel_2d_array") {
    val driver = new DslDriverCCuda[Int, Unit] {
      
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewSharedArray[Int](1, 2)
        x(0)(0) = 0
        printf("%d", (x(0)(1)))
        val y = NewSharedArray[Int](1, 2, 3)
        y(0)(0)(0) = 0
        printf("%d", (y(0)(1)(0)))
        
      }
    }
    System.out.println(indent(driver.code))
  }

  test("transpose_kernel") {
    val driver = new DslDriverCCuda[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val rcount = 32
        val ccount = 32
        val size = rcount * ccount
        val input = NewArray[Int](size)
        val output = NewArray[Int](size)

        for (i <- (0 until size): Rep[Range]) {
          input(i) = i + 1
        }

        for (i <- (0 until rcount): Rep[Range]) {
          for (j <- (0 until ccount): Rep[Range]) {
            printf("%d ", (input(i * rcount + j)))
          }
          printf("\n");
        }

        val transposeKernel = cudaTranspose[Int]
        transposeKernel(input, output, dim3(rcount, ccount), dim3(ccount, rcount))

        for (i <- (0 until rcount): Rep[Range]) {
          for (j <- (0 until ccount): Rep[Range]) {
            printf("%d ", (output(i * rcount + j)))
          }
        }
      }
    }
    System.out.println(indent(driver.code))
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
//    check("softmax", driver.code, "cu")
    System.out.println(indent(driver.code))

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
    System.out.println(indent(driver.code))
  }

  test("kernel_performance") {
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
    check("kernel_performance", driver.code, "cu")
  }
}

