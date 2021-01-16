package lms
package thirdparty.array_computation

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
import lms.collection.mutable.ArrayTypeLess

class CuBLASTest extends TutorialFunSuite {
  val under = "thirdparty/cuda/"

  abstract class DslDriverCCuBLAS[A: Manifest, B: Manifest] extends DslDriverC[A,B] with CudaOps with CuBLASOps{ q =>
    override val codegen = new DslGenC with CCodeGenCudaOps with CCodeGenCuBLASOps {
      val IR: q.type = q
    }
    override val compilerCommand = "nvcc -std=c++11 -O3"

    val curPath = System.getProperty("user.dir")
    override val sourceFile = s"$curPath/snippet.cu"
    override val executable = s"$curPath/snippet"
  }

  test("gemm-geam-test") {
    val driver = new DslDriverCCuBLAS[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        val M = 2
        val K = 2
        val N = 2
        
        // a MxK matrix whose values are 2.0f
        val mat2 = NewArray[Float](M * K)
        for (i <- (0 until M * K): Rep[Range]) {
          mat2(i) = 2.0f
        }

        // a KxN matrix whose values are 3.0f
        val mat3 = NewArray[Float](K * N)
        for (i <- (0 until M * K): Rep[Range]) {
          mat3(i) = 3.0f
        }

        // allocate memories on device
        val devPtrA = cudaMalloc2[Float](M * K)
        cudaCall(cudaMemcpyOfT[Float](devPtrA, mat2, M * K, host2device))
        val devPtrB = cudaMalloc2[Float](K * N)
        cudaCall(cudaMemcpyOfT[Float](devPtrB, mat3, K * N, host2device))
        val devPtrC = cudaMalloc2[Float](M * N)

        val res = NewArray[Float](M * N)

        val handle = cublasHandle
        cublasCall(cublasCreate(handle))

        // test GEMM
        var alpha = 1.0f
        var beta = 0.0f
        cublasCall(cublasSgemm(handle, cublasOpN, cublasOpN, M, N, K, alpha, devPtrA, M, devPtrB, K, beta, devPtrC, M))
        
        cudaCall(cudaMemcpyOfT[Float](res, devPtrC, M * N, device2host))
        printf("Test GEMM:\n")
        for (i <- (0 until M): Rep[Range]) {
          for (j <- (0 until N): Rep[Range]) {
            val index = (j * M) + i
            val value = res(index)
            printf("%f, ", value)
          }
          printf("\n")
        }

        // test GEAM
        alpha = 1.0f
        beta = 1.0f
        cublasCall(cublasSgeam(handle, cublasOpN, cublasOpN, N, M, alpha, devPtrA, M, beta, devPtrB, K, devPtrC, M))

        cudaCall(cudaMemcpyOfT[Float](res, devPtrC, M * N, device2host))
        printf("Test GEAM:\n")
        for (i <- (0 until M): Rep[Range]) {
          for (j <- (0 until N): Rep[Range]) {
            val index = (j * M) + i
            val value = res(index)
            printf("%f, ", value)
          }
          printf("\n")
        }

        // free resources
        cudaCall(cudaFree(devPtrA))
        cudaCall(cudaFree(devPtrB))
        cudaCall(cudaFree(devPtrC))
        cublasCall(cublasDestroy(handle))
      }
    }
    // System.out.println(indent(driver.code))
    check("cublas_gemm_geam", driver.code, "cu")
  }

  test("gemv-test") {
    val driver = new DslDriverCCuBLAS[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        val M = 2
        val K = 2
        val N = 2

        // an array of length M whose values are 1.0f
        val arr1 = NewArray[Float](M)
        for (i <- (0 until M): Rep[Range]) {
          arr1(i) = 1.0f
        }

        // a matrix of size MxK whose values are 2.0f
        val mat2 = NewArray[Float](M * K)
        for (i <- (0 until M * K): Rep[Range]) {
          mat2(i) = 2.0f
        }

        // allocate memories on device
        val devPtrA = cudaMalloc2[Float](M * K)
        cudaCall(cudaMemcpyOfT[Float](devPtrA, mat2, M * K, host2device))
        val devPtrX = cudaMalloc2[Float](K)
        cudaCall(cudaMemcpyOfT[Float](devPtrX, arr1, K, host2device))
        val devPtrY = cudaMalloc2[Float](M)
        cudaCall(cudaMemcpyOfT[Float](devPtrY, arr1, M, host2device))

        val res = NewArray[Float](M)

        val handle = cublasHandle
        cublasCall(cublasCreate(handle))

        // test GEMV
        var alpha = 1.0f
        var beta = 1.0f
        cublasCall(cublasSgemv(handle, cublasOpN, M, K, alpha, devPtrA, M, devPtrX, 1, beta, devPtrY, 1))

        cudaCall(cudaMemcpyOfT[Float](res, devPtrY, M, device2host))
        printf("Test GEMV:\n")
        for (i <- (0 until M): Rep[Range]) {
          val value = res(i)
          printf("%f, ", value)
        }

        // free resources
        cudaCall(cudaFree(devPtrA))
        cudaCall(cudaFree(devPtrX))
        cudaCall(cudaFree(devPtrY))
        cublasCall(cublasDestroy(handle))
      }
    }
    // System.out.println(indent(driver.code))
    check("cublas_gemv", driver.code, "cu")
  }

  test("dot-test") {
    val driver = new DslDriverCCuBLAS[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        val M = 5

        // an array of length M whose values are 1.0f
        val arr1 = NewArray[Float](M)
        for (i <- (0 until M): Rep[Range]) {
          arr1(i) = 1.0f
        }

        // allocate memories on device
        val devPtrA = cudaMalloc2[Float](M)
        cudaCall(cudaMemcpyOfT[Float](devPtrA, arr1, M, host2device))
        val devPtrB = cudaMalloc2[Float](M)
        cudaCall(cudaMemcpyOfT[Float](devPtrB, arr1, M, host2device))
        val devPtrC = cudaMalloc2[Float](1)
        
        val handle = cublasHandle
        cublasCall(cublasCreate(handle))

        // test DOT
        cublasCall(cublasSdot(handle, 10, devPtrA, 1, devPtrB, 1, devPtrC))
        
        val res = NewArray[Float](M)
        cudaCall(cudaMemcpyOfT[Float](res, devPtrC, 1, device2host))
        printf("Test DOT:\n")
        printf("%f\n", res(0))

        // free resources
        cudaCall(cudaFree(devPtrA))
        cudaCall(cudaFree(devPtrB))
        cudaCall(cudaFree(devPtrC))
        cublasCall(cublasDestroy(handle))
      }
    }
    // System.out.println(indent(driver.code))
    check("cublas_dot", driver.code, "cu")
  }
}