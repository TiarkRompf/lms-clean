package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.{SourceContext, RefinedManifest}

import lms.collection.mutable.{StackArrayOps}

trait CudaOps extends Base with RangeOps with SizeTOps with StackArrayOps with CLibs with CudaFunction {
  /* LMS support for cuda + cublas support */
  // 1. support bindings to manual cuda kernels
  // 2. support bindings to cublas library ???
  // 3. support generating cuda kernels (TODO)

  abstract class CudaErrorT
  // Using a manual function in header file to handle CudaErrorT
  def cudaCall(status: Rep[CudaErrorT]) =
    libFunction[Unit]("CUDA_CALL", Unwrap(status))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  // cudaError_t cudaMalloc ( void** devPtr, size_t size )
  def cudaMalloc[T:Manifest](devPtr: Rep[Array[T]], size: Rep[SizeT]) =
    libFunction[CudaErrorT]("cudaMalloc", Unwrap(devPtr), Unwrap(size))(Seq(1), Seq(0), Set(0))
  def cudaMalloc2[T:Manifest](count: Rep[Int])(implicit __pos: SourceContext) = {
    val addr: Rep[Array[T]] = NewArray[T](0) // FIXME(feiw) just need an uninitialized pointer
    cudaCall(cudaMalloc(addr, SizeT(count * sizeOf[T])))
    addr
  }

  // cudaError_t cudaFree ( void* devPtr )
  def cudaFree[T:Manifest](devPtr: Rep[Array[T]]) =
    libFunction[CudaErrorT]("cudaFree", Unwrap(devPtr))(Seq(0), Seq(0), Set[Int]())

  abstract class CudaMemcpyKind
  def host2host = cmacro[CudaMemcpyKind]("cudaMemcpyHostToHost")
  def host2device = cmacro[CudaMemcpyKind]("cudaMemcpyHostToDevice")
  def device2host = cmacro[CudaMemcpyKind]("cudaMemcpyDeviceToHost")
  def device2device = cmacro[CudaMemcpyKind]("cudaMemcpyDeviceToDevice")
  // Direction of the transfer is inferred from the pointer values. Requires unified virtual addressing
  def cpyDefault = cmacro[CudaMemcpyKind]("cudaMemcpyDefault")

  // ​cudaError_t cudaMemcpy ( void* dst, const void* src, size_t count, cudaMemcpyKind kind )
  def cudaMemcpy[T:Manifest](dst: Rep[Array[T]], src: Rep[Array[T]], size: Rep[SizeT], kind: Rep[CudaMemcpyKind]) =
    libFunction[CudaErrorT]("cudaMemcpy", Unwrap(dst), Unwrap(src), Unwrap(size), Unwrap(kind))(Seq(1,2,3), Seq(0), Set[Int]())
  def cudaMemcpyOfT[T:Manifest](dst: Rep[Array[T]], src: Rep[Array[T]], count: Rep[Int], kind: Rep[CudaMemcpyKind])(implicit __pos: SourceContext) =
    cudaMemcpy(dst, src, SizeT(count * sizeOf[T]), kind)

  // CUDA Kernel Basics:

  // CUDA is a scalable parallel programming model and a software environment for parallel computing
  // NVIDIA’s TESLA architecture accelerates CUDA

  // Each thread has an ID that it uses to compute memory addresses and make control decisions (threadID)
  // Cooperation within smaller batches of threads is scalable

  // Kernel launches a grid of thread blocks
  // Threads within a block cooperate via shared memory Threads within a block can synchronize
  // Threads in different blocks cannot cooperate

  // Each Multiprocess has several Thread processors with one shared memory

  // Each Thread has access to Register (on chip memory) and Local Memory (off chip memory, avoid using)
  // Each Block has access to Shared Memory (on chip memory)
  // Each Kernel has access to Global Memory (off chip, persistent, I/O)
  // Host can read and write global memory but not shared memory

  // Threads are executed by thread processors.
  // Thread-blocks are executed on multiprocessors (containing several thread processors and shared memory).
  // Several concurrent thread-blocks can reside on one multiprocessor if the resource is enough
  // A kernel is launched as a grid of thread blocks.
  // Only one kernel can execute on a device at a time.

  // Kernels are C functions with some restrictions
  // 1. Cannot access host memory
  // 2. Must have void return type
  // 3. No variable number of arguments (“varargs”) Not recursive
  // 4. No static variables
  // Function arguments automatically copied from host to device

  // Kernels designated by function qualifier: __global__
  // Function called from host and executed on device。 Must return void

  // Other CUDA function qualifiers：__device__
  // Function called from device and run on device。Cannot be called from host code

  // __host__: Function called from host and executed on host (default)
  // __host__ and __device__ qualifiers can be combined to generate both CPU and GPU code

  // Modified C function call syntax: kernel<<<dim3 dG, dim3 dB>>>(...)
  // Execution Configuration (“<<< >>>”)
  // dG - dimension and size of grid in blocks Two-dimensional: x and y
  //   Blocks launched in the grid: dG.x * dG.y
  // dB - dimension and size of blocks in threads: Three-dimensional: x, y, and z
  //   Threads per block: dB.x * dB.y * dB.z
  //          Unspecified dim3 fields initialize to 1

  // example:
    // dim3 grid, block;
    // grid.x = 2; grid.y = 4; block.x = 8; block.y = 16;
    // // or equvilently: dim3 grid(2, 4), block(8,16);
    // kernel<<<grid, block>>>(...);

  // kernel<<<32,512>>>(...); 1D dG or dB can be just int

  // All __global__ and __device__ functions have access to these automatically defined variables
  //   dim3 gridDim;  Dimensions of the grid in blocks (at most 2D)
  //   dim3 blockDim; Dimensions of the block in threads
  //   dim3 blockIdx; Block index within the grid
  //   dim3 threadIdx; Thread index within the block
  // Note: Grid contains Blocks, Blocks contains Threads.

  // All kernel launches are asynchronous
    // control returns to CPU immediately
    // kernel executes after all previous CUDA calls have completed
  // cudaMemcpy() is synchronous
    // control returns to CPU after copy completes
    // copy starts after all previous CUDA calls have completed
  // cudaThreadSynchronize()
    // blocks until all previous CUDA calls complete

  // Variable Qualifiers (GPU code)
  // __device__: Stored in global memory (large, high latency, no cache)
  // Allocated with cudaMalloc (__device__ qualifier implied)
  // Accessible by all threads
  // Lifetime: application
  // __shared__: Stored in on-chip shared memory (very low latency)
  // Specified by execution configuration or at compile time
  // Accessible by all threads in the same thread block
  // Lifetime: thread block
  // Unqualified variables:
  // Scalars and built-in vector types are stored in registers
  // What doesn’t fit in registers spills to “local” memory

  // Using shared memory
  // Size known at compile time
  // __global__ void kernel(...) {
  //   ...
  //   __shared__ float sData[256]; ...
  // }
  // int main(void) {
  //   ...
  //   kernel<<<nBlocks,blockSize>>>(...); ...
  // }

  // Size known at kernel launch
  // __global__ void kernel(...) {
  //   ...
  //   extern __shared__ float sData[]; ...
  // }
  // int main(void) {
  //   ...
  //   smBytes = blockSize*sizeof(float);
  //   kernel<<<nBlocks, blockSize, smBytes>>>(...);
  //   ...
  // }

  // GPU Thread Synchronization
  // void __syncthreads();
  // Synchronizes all threads in a block: No thread can pass this barrier until all threads in the block reach it

  // All CUDA calls return error code:
  // Except for kernel launches cudaError_t type
  // cudaError_t cudaGetLastError(void)
  // Returns the code for the last error (no error has a code) Can be used to get error from kernel execution
  // char* cudaGetErrorString(cudaError_t code)
  // Returns a null-terminated character string describing the error
  // printf(“%s\n”, cudaGetErrorString( cudaGetLastError() ) );


  // Some global values for our GPU
  val gridSize = 28
  val blockSize = 512
  val basic_config = Seq(Backend.Const(gridSize), Backend.Const(blockSize))

  // How to bind to manually written kernels (see cuda_header.h)
  // this CUDA kernel fills a value to the cuda array
  def cudaArrayFill[T:Manifest](res: Rep[Array[T]], value: Rep[T], size: Rep[Int]) =
    cudaFunction[Unit]("arrayFill", basic_config, Unwrap(res), Unwrap(value), Unwrap(size))(Seq[Int](), Seq(0), Set[Int]())

  // this CUDA kernel clips the abstract value of the cuda array
  def cudaArrayClipAt[T:Manifest](res: Rep[Array[T]], bound: Rep[T], size: Rep[Int]) =
    cudaFunction[Unit]("clipAt", basic_config, Unwrap(res), Unwrap(bound), Unwrap(size))(Seq(0), Seq(0), Set[Int]())

  abstract class Dim3
  def dim3(a: Rep[Int], b: Rep[Int] = unit(1), c: Rep[Int] = unit(1)): Rep[Dim3] =
    libFunction("dim3", Unwrap(a), Unwrap(b), Unwrap(c))(Seq[Int](), Seq[Int](), Set[Int](), Backend.UNSAFE)


  // How do we generate the kernels (instead of manually writing them)
  // the cuda functions need Dim3 typed inputs :)
  def cudaGlobalFun[A:Manifest, B:Manifest](f: Rep[A] => Rep[B]) =
    Wrap[(A,Dim3,Dim3)=>B](__topFun(f, 1, xn => Unwrap(f(Wrap[A](xn(0)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C])=
    Wrap[(A,B,Dim3,Dim3)=>C](__topFun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]) =
    Wrap[(A,B,C,Dim3,Dim3)=>D](__topFun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]) =
    Wrap[(A,B,C,D,Dim3,Dim3)=>E](__topFun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => Rep[F]) =
    Wrap[(A,B,C,D,E,Dim3,Dim3)=>F](__topFun(f, 5, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F]) => Rep[G]) =
    Wrap[(A,B,C,D,E,F,Dim3,Dim3)=>G](__topFun(f, 6, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)))), "__global__"))

  // When coding kernel functions, we often need some kernel variables
  def gridDimX = cmacro[Int]("gridDim.x")
  def gridDimY = cmacro[Int]("gridDim.y")
  def blockDimX = cmacro[Int]("blockDim.x")
  def blockDimY = cmacro[Int]("blockDim.y")
  def blockDimZ = cmacro[Int]("blockDim.z")

  def blockIdxX = cmacro[Int]("blockIdx.x")
  def blockIdxY = cmacro[Int]("blockIdx.y")
  def threadIdxX = cmacro[Int]("threadIdx.x")
  def threadIdxY = cmacro[Int]("threadIdx.y")
  def threadIdxZ = cmacro[Int]("threadIdx.z")

}

trait CCodeGenCudaOps extends CCodeGenSizeTOps with CudaCodeGenLibFunction with CCodeGenLibs {
  // need to register the headers
  registerHeader("<cuda_header.h>")

  override def remap(m: Manifest[_]): String = m.runtimeClass.getName match {
    case s: String if s.endsWith("Dim3") => "dim3"
    case _ => super.remap(m)
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s,"@", (f:Backend.Exp)::args, _) if ((graphCache(f.asInstanceOf[Sym]) match {
      case Node(_, "λ", (b: Block)::Backend.Const(0)::Backend.Const("__global__")::Nil, _) => true
      case _ => false
    })) =>
      shallowP(f);
      assert(args.size > 1, "size of args should be at least 2")
      val dims = args.drop(args.length - 2)
      val other_args = args.dropRight(2)
      emit("<<<"); shallow(dims(0)); emit(", "); shallow(dims(1)); emit(">>>");
      emit("("); other_args.headOption.foreach(h => { shallowP(h, 0); other_args.tail.foreach(a => { emit(", "); shallowP(a, 0) }) }); emit(")")

    case _ => super.shallow(n)
  }

}
