package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import macros.SourceContext
import lms.collection._


class CudaTest extends TutorialFunSuite {
  val under = "thirdparty/cuda"

  // first: get a driver :)
  abstract class DslDriverCCuda[A: Manifest, B: Manifest] extends DslDriverC[A,B] with CudaOps { q =>
    override val codegen = new DslGenC with CCodeGenCudaOps {
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
        printf("%d %d", arr(2), arr(3))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("cuda_global_fill", driver.code, "cu")
  }

  test("fill_gen_kernel2") {
    val driver = new DslDriverCCuda[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val cuda_arr = cudaMalloc2[Float](5)
        val cudaFillFloat = CUDA_FILL(manifest[Float])
        val array = new ARRAY(Unwrap(cuda_arr))
        cudaFillFloat(array, 3.0f, 5, dim3(gridSize), dim3(blockSize))

        val arr = NewArray[Float](5)
        cudaCall(cudaMemcpyOfT(arr, cuda_arr, 5, device2host))
        printf("%d %d", arr(2), arr(3))
        cudaCall(cudaFree(cuda_arr))
      }
    }
    check("cuda_global_fill", driver.code, "cu")
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

}

