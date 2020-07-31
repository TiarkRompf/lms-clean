package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.{SourceContext, RefinedManifest}

import lms.collection.mutable.{StackArrayOps}

trait CudaOps extends Base with SizeTOps with StackArrayOps with CLibs with CudaFunction {
  /* LMS support for cuda + cublas support */
  // 1. support bindings to manual cuda kernels
  // 2. support bindings to cublas library ???
  // 3. support generating cuda kernels (TODO)

  abstract class CudaErrorT
  // Using a manual function in header file to handle CudaErrorT
  def cudaCall(status: Rep[CudaErrorT]) =
    libFunction[Unit]("CUDA_CALL", Unwrap(status))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  // cudaError_t cudaMalloc ( void** devPtr, size_t size )
  def cudaMalloc[T: Manifest](devPtr: Rep[Array[T]], size: Rep[SizeT]) =
    libFunction[CudaErrorT]("cudaMalloc", Unwrap(devPtr), Unwrap(size))(Seq(1), Seq(0), Set(0))
  def cudaMalloc2[T: Manifest](count: Rep[Int])(implicit __pos: SourceContext) = {
    val addr: Rep[Array[T]] = NewArray[T](0) // FIXME(feiw) just need an uninitialized pointer
    cudaCall(cudaMalloc(addr, SizeT(count * sizeOf[T])))
    addr
  }

  // cudaError_t cudaFree ( void* devPtr )
  def cudaFree[T: Manifest](devPtr: Rep[Array[T]]) =
    libFunction[CudaErrorT]("cudaFree", Unwrap(devPtr))(Seq(0), Seq(0), Set[Int]())

  abstract class CudaMemcpyKind
  def host2host = cmacro[CudaMemcpyKind]("cudaMemcpyHostToHost")
  def host2device = cmacro[CudaMemcpyKind]("cudaMemcpyHostToDevice")
  def device2host = cmacro[CudaMemcpyKind]("cudaMemcpyDeviceToHost")
  def device2device = cmacro[CudaMemcpyKind]("cudaMemcpyDeviceToDevice")
  // Direction of the transfer is inferred from the pointer values. Requires unified virtual addressing
  def cpyDefault = cmacro[CudaMemcpyKind]("cudaMemcpyDefault")

  // ​cudaError_t cudaMemcpy ( void* dst, const void* src, size_t count, cudaMemcpyKind kind )
  def cudaMemcpy[T: Manifest](dst: Rep[Array[T]], src: Rep[Array[T]], size: Rep[SizeT], kind: Rep[CudaMemcpyKind]) =
    libFunction[CudaErrorT]("cudaMemcpy", Unwrap(dst), Unwrap(src), Unwrap(size), Unwrap(kind))(
      Seq(1, 2, 3),
      Seq(0),
      Set[Int]()
    )
  def cudaMemcpyOfT[T: Manifest](dst: Rep[Array[T]], src: Rep[Array[T]], count: Rep[Int], kind: Rep[CudaMemcpyKind])(
      implicit __pos: SourceContext
  ) =
    cudaMemcpy(dst, src, SizeT(count * sizeOf[T]), kind)
}

trait CCodeGenCudaOps extends CCodeGenSizeTOps with CCodeGenLibs {
  // need to register the headers
  registerHeader("<cuda_header.h>")
}
