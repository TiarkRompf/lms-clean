package lms.thirdparty.array_computation

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.collection.mutable.{ArrayOps, ArrayTypeLess, StackArrayOps}
import lms.transformation.tensor.FixedSizeTensorDeviceTypeLess
import lms.thirdparty.{CCodeGenLibs, CCodeGenSizeTOps, CLibTypeLess, CLibs, SIZE_TTypeLess, SizeTOps}


object CUDATypeLess extends Dsl with StackArrayOps with CLibs with CudaFunction {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import FixedSizeTensorDeviceTypeLess._
  import CLibTypeLess._
  import SIZE_TTypeLess._
  import RangeTypeLess._

  // FIXME(feiw) hacky temp status type (used to be CudaErrorT)
  def CUDA_CALL(status: Backend.Exp) =
    libFunction[Unit]("CUDA_CALL", status)(Seq[Int](0), Seq[Int](), Set[Int](), Adapter.CTRL)

  // a typeless interface for CUDA_MALLOC
  def CUDA_MALLOC(count: INT, m: Manifest[_])(implicit __pos: SourceContext): ARRAY = {
    val addr = ARRAY(0, m)
    CUDA_CALL(Unwrap(libFunction[Any]("cudaMalloc", addr.x, SIZE_T(count * SIZE_OF(m)).x)(Seq(1), Seq(0), Set(0))))
    addr
  }

  def CUDA_MALLOC_BYTES(count: INT, m: Manifest[_])(implicit __pos: SourceContext): ARRAY = {
    val addr = ARRAY(0, m)
    CUDA_CALL(Unwrap(libFunction[Any]("cudaMalloc", addr.x, SIZE_T(count).x)(Seq(1), Seq(0), Set(0))))
    addr
  }

  // a typeless interface for CUDA_FREE
  def CUDA_FREE(devPtr: ARRAY) =
    CUDA_CALL(Unwrap(libFunction[Any]("cudaFree", devPtr.x)(Seq(0), Seq(0), Set[Int]())))

  // FIXME(feiw) hacky Macro type (used to be `abstract class CudaMemcpyKind`)
  class CUDA_MEMCPY_KIND(override val x: Backend.Exp) extends TOP(x)
  def CUDA_MEMCPY_KIND(x: TOP): CUDA_MEMCPY_KIND = new CUDA_MEMCPY_KIND(x.x)
  def HOST2HOST(implicit __pos: SourceContext): CUDA_MEMCPY_KIND = CUDA_MEMCPY_KIND(CMACRO("cudaMemcpyHostToHost", manifest[Any]))
  def HOST2DEVICE(implicit __pos: SourceContext): CUDA_MEMCPY_KIND = CUDA_MEMCPY_KIND(CMACRO("cudaMemcpyHostToDevice", manifest[Any]))
  def DEVICE2HOST(implicit __pos: SourceContext): CUDA_MEMCPY_KIND = CUDA_MEMCPY_KIND(CMACRO("cudaMemcpyDeviceToHost", manifest[Any]))
  def DEVICE2DEVICE(implicit __pos: SourceContext): CUDA_MEMCPY_KIND = CUDA_MEMCPY_KIND(CMACRO("cudaMemcpyDeviceToDevice", manifest[Any]))
  // Direction of the transfer is inferred from the pointer values. Requires unified virtual addressing
  def CPYDefault(implicit __pos: SourceContext): CUDA_MEMCPY_KIND = CUDA_MEMCPY_KIND(CMACRO("cudaMemcpyDefault", manifest[Any]))

  // ​cudaError_t cudaMemcpy ( void* dst, const void* src, size_t count, cudaMemcpyKind kind )
  def CUDA_MEMCPY(dst: ARRAY, src: ARRAY, count: INT, kind: CUDA_MEMCPY_KIND, m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_CALL(Unwrap(libFunction[Any]("cudaMemcpy", dst.x, src.x, SIZE_T(count * SIZE_OF(m)).x, kind.x)(Seq(1,2,3), Seq(0), Set[Int]())))

  // cudaSetDevice(int)
  def CUDA_SET_DEVICE(device: INT) =
    CUDA_CALL(Unwrap(libFunction[Any]("cudaSetDevice", device.x)(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)))

  def CUDA_STREAM_SYNCHRONIZE(stream: TOP) =
    CUDA_CALL(Unwrap(libFunction[Any]("cudaStreamSynchronize", stream.x)(Seq(0), Seq(0), Set[Int]())))

  class DIM3(override val x: Backend.Exp) extends TOP(x)
  def DIM3(a: Int, b: Int = 1, c: Int = 1)(implicit __pos: SourceContext): DIM3 =
    (new DIM3(Unwrap(libFunction("dim3", Backend.Const(a), Backend.Const(b), Backend.Const(c))(Seq[Int](),
      Seq[Int](), Set[Int](), Backend.UNSAFE)))).withSource(__pos)

  def CUDA_KERNEL3(f: List[Backend.Exp] => Backend.Exp, ms: Manifest[_]*)(implicit __pos: SourceContext) = {
    val kernel = Adapter.g.reflect("λ", Adapter.g.reify(3, f), Backend.Const(3), Backend.Const("__global__"))
    (a: TOP, b: TOP, c: TOP, dim1: DIM3, dim2: DIM3) => {
      // type checking
      require(ms.toSeq.length == 3, s"should have 3 manifests, but got ${ms.toSeq.length}")
      Seq(a, b, c).zip(ms).zipWithIndex.foreach {
        case ((arg, man), index) => require(arg.x.isInstanceOf[Backend.Const] || arg.t == man,
          s"mismatched type for ${index}th argument. Provided: ${arg.x} ${arg.t} Required: $man")
      }
      UNIT(Adapter.g.reflect("@", kernel, a.x, b.x, c.x, dim1.x, dim2.x))
    }
  }
  def CUDA_KERNEL4(f: List[Backend.Exp] => Backend.Exp, ms: Manifest[_]*)(implicit __pos: SourceContext) = {
    val kernel = Adapter.g.reflect("λ", Adapter.g.reify(4, f), Backend.Const(4), Backend.Const("__global__"))
    (a: TOP, b: TOP, c: TOP, d: TOP, dim1: DIM3, dim2: DIM3) => {
      // type checking
      require(ms.toSeq.length == 4, s"should have 4 manifests, but got ${ms.toSeq.length}")
      Seq(a, b, c, d).zip(ms).zipWithIndex.foreach {
        case ((arg, man), index) => require(arg.x.isInstanceOf[Backend.Const] || arg.t == man,
          s"mismatched type for ${index}th argument. Provided: ${arg.x} ${arg.t} Required: $man")
      }
      UNIT(Adapter.g.reflect("@", kernel, a.x, b.x, c.x, d.x, dim1.x, dim2.x))
    }
  }
  def CUDA_KERNEL5(f: List[Backend.Exp] => Backend.Exp, ms: Manifest[_]*)(implicit __pos: SourceContext) = {
    val kernel = Adapter.g.reflect("λ", Adapter.g.reify(5, f), Backend.Const(5), Backend.Const("__global__"))
    (a: TOP, b: TOP, c: TOP, d: TOP, e: TOP, dim1: DIM3, dim2: DIM3) => {
      // type checking
      require(ms.toSeq.length == 5, s"should have 5 manifests, but got ${ms.toSeq.length}")
      Seq(a, b, c, d, e).zip(ms).zipWithIndex.foreach {
        case ((arg, man), index) =>
          require(arg.x.isInstanceOf[Backend.Const] || arg.t == man,
            s"mismatched type for ${index}th argument. Provided: ${arg.x} ${arg.t} Required: $man")
      }
      UNIT(Adapter.g.reflect("@", kernel, a.x, b.x, c.x, d.x, e.x, dim1.x, dim2.x))
    }
  }


  // When coding kernel functions, we often need some kernel variables
  def gridDimX(implicit __pos: SourceContext): INT = INT(CMACRO("gridDim.x", manifest[Int]))
  def gridDimY(implicit __pos: SourceContext): INT = INT(CMACRO("gridDim.y", manifest[Int]))
  def gridDimZ(implicit __pos: SourceContext): INT = INT(CMACRO("gridDim.z", manifest[Int]))
  def blockDimX(implicit __pos: SourceContext): INT = INT(CMACRO("blockDim.x", manifest[Int]))
  def blockDimY(implicit __pos: SourceContext): INT = INT(CMACRO("blockDim.y", manifest[Int]))
  def blockDimZ(implicit __pos: SourceContext): INT = INT(CMACRO("blockDim.z", manifest[Int]))

  def blockIdxX(implicit __pos: SourceContext): INT = INT(CMACRO("blockIdx.x", manifest[Int]))
  def blockIdxY(implicit __pos: SourceContext): INT = INT(CMACRO("blockIdx.y", manifest[Int]))
  def blockIdxZ(implicit __pos: SourceContext): INT = INT(CMACRO("blockIdx.z", manifest[Int]))
  def threadIdxX(implicit __pos: SourceContext): INT = INT(CMACRO("threadIdx.x", manifest[Int]))
  def threadIdxY(implicit __pos: SourceContext): INT = INT(CMACRO("threadIdx.y", manifest[Int]))
  def threadIdxZ(implicit __pos: SourceContext): INT = INT(CMACRO("threadIdx.z", manifest[Int]))

  def blockRows(implicit __pos: SourceContext): INT = INT(CMACRO("BLOCK_ROWS", manifest[INT]))

  val gridSize = 28
  val blockSize = 512
  val tileDim = 32

  def CUDA_FILL_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_KERNEL3({xn: List[Backend.Exp] =>
    withComment(s"generating kernel function for FILL of type $m") {
      // type cast
      val array = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val value = (new NUM(xn(1))).withSrcType(__pos, m)
      val size  = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int](size.x), Wrap[Int](stride.x))) {
        array(INT(Unwrap(i))) = value; ()
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m, manifest[Int])


  def CUDA_ELEMENTWISE_MUTATION_BINARY_KERNEL(m: Manifest[_], op: (NUM, NUM) => NUM, comment: String = "")(implicit __pos: SourceContext) = CUDA_KERNEL3({xn: List[Backend.Exp] =>
    withComment(comment) {
      // type cast
      val base = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val other = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val size = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int](size.x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        base(index) = op(base(index), other(index)); ()
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, manifest[Int])


  // Element-wise Accumulation (+=)
  def CUDA_ACCUM_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_ELEMENTWISE_MUTATION_BINARY_KERNEL(m, _ + _, s"generating kernel function for ACCUM of type $m")


  // M_I_M means that there are 3 input tensors, and the first and last tensors are mutated
  // that is why it is named `M_I_M` kernel, for Mutate_Input_Mutate Kernel
  def CUDA_ELEMENTWISE_M_I_M_KERNEL(m: Manifest[_], op: (NUM, NUM, NUM) => (NUM, NUM), comment: String = "")(implicit __pos: SourceContext) = CUDA_KERNEL4({xn: List[Backend.Exp] =>
    withComment(comment) {
      // type cast
      val t0 = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val t1 = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val t2 = (new ARRAY(xn(2))).withSrcType(__pos, m.arrayManifest)
      val size = (new INT(xn(3))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int](size.x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        val (t0_new, t2_new) = op(t0(index), t1(index), t2(index))
        t0(index) = t0_new
        t2(index) = t2_new; ()
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, m.arrayManifest, manifest[Int])

  def CUDA_TRANSPOSE_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_KERNEL3({xn: List[Backend.Exp] =>
    withComment(s"generating kernel function for TRANS of type $m") {
      // type cast
      val in = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val out = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val size = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

      val x = blockIdxX * tileDim + threadIdxX
      val y = blockIdxY * tileDim + threadIdxY
      val width = gridDimX * tileDim

      val start = Backend.Const(0)
      for (i <- range_until_step(Wrap[Int](start), Wrap[Int](tileDim.x), Wrap[Int](blockSize.x))) {
        val rhead = INT((y + INT(Unwrap(i))) * width + x)
        val whead = INT(x * width + (y + INT(Unwrap(i))))
        out(whead) = in(rhead); ()
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, manifest[Int])


  // Simple SGD Nesterov (https://github.com/pytorch/pytorch/blob/master/torch/optim/sgd.py)
  def CUDA_SGD_Nesterov_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_ELEMENTWISE_M_I_M_KERNEL(m, (w: NUM, g: NUM, v: NUM) => {
      // default \mu as 0.5 and learning rate to 0.0001 for now
      val mu = 0.5f
      val lr = 0.0001f
      val v_1 = v * FLOAT(mu) + g
      val w_1 = w - v_1 * FLOAT(lr)
      (w_1, v_1)
    }, s"generating kernel function for SGD of type $m")


  def CUDA_ELEMENTWISE_BINARY_KERNEL(m: Manifest[_], op: (NUM, NUM) => NUM, comment: String = "")(implicit __pos: SourceContext) = CUDA_KERNEL4({xn: List[Backend.Exp] =>
    withComment(comment) {
      // type cast
      val a = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val b = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val res = (new ARRAY(xn(2))).withSrcType(__pos, m.arrayManifest)
      val size = (new INT(xn(3))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int](size.x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        res(index) = op(a(index), b(index)); ()
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, m.arrayManifest, manifest[Int])


  // Element-wise Unary Operation
  def CUDA_UNARY_KERNEL(m: Manifest[_], op: (NUM) => NUM, comment: String = "")(implicit __pos: SourceContext) = CUDA_KERNEL3({xn: List[Backend.Exp] =>
    withComment(comment) {
      // type cast
      val in = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val out = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val size = (new INT(xn(2))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int](size.x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        out(index) = op(in(index)); ()
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, manifest[Int])

  def CUDA_NEGATE_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_UNARY_KERNEL(m, NUM_ZERO(m) - _, s"generating kernel function for NEGATE of type $m")

  def CUDA_INVERT_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_UNARY_KERNEL(m, NUM_ONE(m) / _, s"generating kernel function for INVERT of type $m")

  def CUDA_TANH_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_UNARY_KERNEL(m, _ tanh, s"generating kernel function for TANH of type $m")

  def CUDA_TANH_GRAD_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ tanh_grad _, s"generating kernel function for TANH_GRAD of type $m")

  def CUDA_RELU_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_UNARY_KERNEL(m, NUM_ZERO(m) max _, s"generating kernel function for RELU of type $m")

  def CUDA_RELU_GRAD_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ relu_grad _, s"generating kernel function for RELU_GRAD of type $m")

  def CUDA_INVERT_GRAD_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ invert_grad _, s"generating kernel function for INVERT_GRAD of type $m")

  // Element-wise Add
  def CUDA_ADD_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ + _, s"generating kernel function for ADD of type $m")

  def CUDA_ARRAY_ADD(a: ARRAY, b: ARRAY, res: ARRAY, size: INT)(implicit __pos: SourceContext) = {
    val kernel = CUDA_ADD_KERNEL(a.et)
    kernel(a, b, res, size, DIM3(gridSize), DIM3(blockSize))
  }


  // Element-wise Minus
  def CUDA_MINUS_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ - _, s"generating kernel function for MINUS of type $m")

  def CUDA_ARRAY_MINUS(a: ARRAY, b: ARRAY, res: ARRAY, size: INT)(implicit __pos: SourceContext) = {
    val kernel = CUDA_MINUS_KERNEL(a.et)
    kernel(a, b, res, size, DIM3(gridSize), DIM3(blockSize))
  }


  // Element-wise Mult
  def CUDA_MULT_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ * _, s"generating kernel function for MULT of type $m")

  def CUDA_ARRAY_MULT(a: ARRAY, b: ARRAY, res: ARRAY, size: INT)(implicit __pos: SourceContext) = {
    val kernel = CUDA_MULT_KERNEL(a.et)
    kernel(a, b, res, size, DIM3(gridSize), DIM3(blockSize))
  }


  // Element-wise Div
  def CUDA_DIV_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) =
    CUDA_ELEMENTWISE_BINARY_KERNEL(m, _ / _, s"generating kernel function for DIV of type $m")

  def CUDA_ARRAY_DIV(a: ARRAY, b: ARRAY, res: ARRAY, size: INT)(implicit __pos: SourceContext) = {
    val kernel = CUDA_DIV_KERNEL(a.et)
    kernel(a, b, res, size, DIM3(gridSize), DIM3(blockSize))
  }

  // This split op kernel has lots of limitations
  // 1. the input (0th parameter) is split to 2 outputs (1st and 2nd parameter)
  // 2. the input is 2D, and the split axis is 1
  // 3. the split is by equal sizes, so the input dim1 size must be even number
  // 4. the 3rd input is size of dim 0 of input, the 4th input is 1/2 of size of dim 1 of input
  def CUDA_SPLIT2_2D1_EQUAL_KERNEL(m: Manifest[_], comment: String = "")(implicit __pos: SourceContext) = CUDA_KERNEL5({xn: List[Backend.Exp] =>
    withComment(comment) {
      // type cast
      val input = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val output0 = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val output1 = (new ARRAY(xn(2))).withSrcType(__pos, m.arrayManifest)
      val dim0 = (new INT(xn(3))).withSrcType(__pos, manifest[Int])
      val dim1 = (new INT(xn(4))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int]((dim0*dim1*2).x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        val size0 = index / (dim1 * 2)
        val size1 = index % (dim1 * 2)
        __ifThenElse(Wrap[Boolean]((size1 < dim1).x),
          { output0(INT(size0 * dim1 + size1)) = input(index); () },
          { output1(INT(size0 * dim1 + size1 - dim1)) = input(index); () })
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, m.arrayManifest, manifest[Int], manifest[Int])

  // This concat op kernel has lots of limitations
  // 1. it concats 2 inputs
  // 2. the inputs are 2D, and the split axis is 1
  // 3. the inputs is by equal sizes
  // 4. the 3rd input is size of dim 0 of input0, the 4th input is size of dim 1 of input0
  def CUDA_CONCAT2_2D1_EQUAL_KERNEL(m: Manifest[_], comment: String = "")(implicit __pos: SourceContext) = CUDA_KERNEL5({xn: List[Backend.Exp] =>
    withComment(comment) {
      // type cast
      val input0 = (new ARRAY(xn(0))).withSrcType(__pos, m.arrayManifest)
      val input1 = (new ARRAY(xn(1))).withSrcType(__pos, m.arrayManifest)
      val output = (new ARRAY(xn(2))).withSrcType(__pos, m.arrayManifest)
      val dim0 = (new INT(xn(3))).withSrcType(__pos, manifest[Int])
      val dim1 = (new INT(xn(4))).withSrcType(__pos, manifest[Int])

      // actual computation
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int]((dim0*dim1).x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        val size0 = index / dim0
        val size1 = index % dim0
        __ifThenElse(Wrap[Boolean]((size1 < dim1).x),
          { output(index) = input0(INT(size0 * dim1 + size1)); () },
          { output(index) = input1(INT(size0 * dim1 + size1 - dim1)); ()})
      }
      Backend.Const(())
    }
  }, m.arrayManifest, m.arrayManifest, m.arrayManifest, manifest[Int], manifest[Int])

}


trait CudaOps extends Dsl with StackArrayOps with SizeTOps with CLibs with CudaFunction {
  /* LMS support for cuda + cublas support */
  // 1. support bindings to manual cuda kernels
  // 2. support bindings to cublas library (TODO)
  // 3. support generating cuda kernels (TODO)

  class CudaErrorT
  // Using a manual function in header file to handle CudaErrorT
  def cudaCall(status: Rep[CudaErrorT]) =
    libFunction[Unit]("CUDA_CALL", Unwrap(status))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  def cudaArrayEffect[T](arr: Rep[Array[T]], idx: Rep[Int], rhs: Rep[T]) =
    Wrap[Unit](Adapter.g.reflectEffect("cudaArrayEffect", Unwrap(arr), Unwrap(idx), Unwrap(rhs))(Unwrap(arr), Unwrap(idx), Unwrap(rhs))(Adapter.CTRL))

  def cudaSyncThreads =
    libFunction[Unit]("__syncthreads")(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  // warp size macro
  def warpSize = cmacro[Int]("NVIDIA_WARP_SIZE")

  // macro for infinity
  def infinity[N:Numeric:Manifest]: Rep[N] = manifest[N] match {
    case a if a == manifest[Float] => cmacro[N]("INFINITY")
    case a if a == manifest[Int] => cmacro[N]("INT_MAX")
    case a if a == manifest[Long] => cmacro[N]("LONG_MAX")
    case _ => ???
  }

  // cudaError_t cudaMalloc ( void** devPtr, size_t size )
  def cudaMalloc[T:Manifest](devPtr: Rep[Array[T]], size: Rep[SizeT]) =
    libFunction[CudaErrorT]("cudaMalloc", Unwrap(devPtr), Unwrap(size))(Seq(1), Seq(0), Set(0))
  def cudaMalloc2[T:Manifest](count: Rep[Int])(implicit __pos: SourceContext) = {
    val addr: Rep[Array[T]] = NewArray[T](0) // FIXME(feiw) just need an uninitialized pointer
    cudaCall(cudaMalloc(addr, SizeT(count * sizeOf[T])))
    addr
  }
  def cudaMalloc3[T:Manifest](devPtr: Rep[Array[T]], count: Rep[Int])(implicit __pos: SourceContext) = {
    libFunction[CudaErrorT]("cudaMalloc", Unwrap(devPtr), Unwrap(SizeT(count * sizeOf[T])))(Seq(1), Seq(0), Set(0))
  }
  def cudaMalloc2BySize[T:Manifest](size: Rep[SizeT])(implicit __pos: SourceContext) = {
    val addr: Rep[Array[T]] = NewArray[T](0) // FIXME(feiw) just need an uninitialized pointer
    cudaCall(cudaMalloc(addr, size))
    addr
  }

  def NewSharedArray[T:Manifest](x: Rep[Int]): Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("NewSharedArray", Unwrap(x)))
  }

  def CastArray[Original:Manifest, New:Manifest](arr: Rep[Array[Original]]) = {
    Wrap[Array[New]](Adapter.g.reflectMutable("CastArray", Unwrap(arr)))
  }

  def cudaBallotSync[T:Manifest](mask: Rep[Int], predicate: Rep[Int]) =
    libFunction[Int]("__ballot_sync", Unwrap(mask), Unwrap(predicate))(Seq[Int](), Seq[Int](), Set[Int]())
  
  def cudaFfs[T:Manifest](x: Rep[Int]) =
    libFunction[Int]("__ffs", Unwrap(x))(Seq[Int](), Seq[Int](), Set[Int]())

  case class Matrix2D[T:Manifest](x: Rep[Array[T]], skip: Int) {
    def apply(i: Rep[Int], j: Rep[Int])(implicit __pos: SourceContext): Rep[T] = {
      val offset = skip * i + j
      Wrap[T](Adapter.g.reflectRead("array_get", Unwrap(x), Unwrap(offset))(Unwrap(x)))
    }
    def apply(is: List[Rep[Int]])(implicit __pos: SourceContext): Rep[T] = {
      require(is.length == 2)
      apply(is(0), is(1))
    }
    def update(i: Rep[Int], j: Rep[Int], y: Rep[T])(implicit __pos: SourceContext): Unit = {
      val offset = skip * i + j
      Adapter.g.reflectWrite("array_set", Unwrap(x), Unwrap(offset), Unwrap(y))(Unwrap(x))
    }
    def update(is: List[Rep[Int]], y: Rep[T])(implicit __pos: SourceContext): Unit = {
      require(is.length == 2)
      update(is(0), is(1), y)
    }
  }
  def NewSharedArray[T:Manifest](x: Int, y: Int): Matrix2D[T] = {
    Matrix2D(Wrap[Array[T]](Adapter.g.reflectMutable("NewSharedArray", Unwrap(x * y))), y)
  }

  case class Matrix3D[T:Manifest](x: Rep[Array[T]], skip0: Int, skip1: Int) {
    def apply(i: Rep[Int], j: Rep[Int], k: Rep[Int])(implicit __pos: SourceContext): Rep[T] = {
      val offset = skip0 * i + skip1 * j + k
      Wrap[T](Adapter.g.reflectRead("array_get", Unwrap(x), Unwrap(offset))(Unwrap(x)))
    }
    def apply(is: List[Rep[Int]])(implicit __pos: SourceContext): Rep[T] = {
      require(is.length == 3)
      apply(is(0), is(1), is(2))
    }
    def update(i: Rep[Int], j: Rep[Int], k: Rep[Int], y: Rep[T])(implicit __pos: SourceContext): Unit = {
      val offset = skip0 * i + skip1 * j + k
      Adapter.g.reflectWrite("array_set", Unwrap(x), Unwrap(offset), Unwrap(y))(Unwrap(x))
    }
    def update(is: List[Rep[Int]], y: Rep[T])(implicit __pos: SourceContext): Unit = {
      require(is.length == 3)
      update(is(0), is(1), is(2), y)
    }
  }
  def NewSharedArray[T:Manifest](x: Int, y: Int, z: Int): Matrix3D[T] = {
    Matrix3D(Wrap[Array[T]](Adapter.g.reflectMutable("NewSharedArray", Unwrap(x * y * z))), y * z, z)
  }

  def NewDynSharedArray[T:Manifest]: Rep[Array[T]] = {
    Wrap[Array[T]](Adapter.g.reflectMutable("NewDynSharedArray"))
  }

  // cudaError_t cudaFree ( void* devPtr )
  def cudaFree[T:Manifest](devPtr: Rep[Array[T]]) =
    libFunction[CudaErrorT]("cudaFree", Unwrap(devPtr))(Seq(0), Seq(0), Set[Int]())

  class CudaMemcpyKind
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

  // cudaSetDevice(int)
  def cudaSetDevice(device: Rep[Int]) =
    libFunction[CudaErrorT]("cudaSetDevice", Unwrap(device))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  class cudaStreamT
  def cudaStream: Rep[cudaStreamT] = newStruct[cudaStreamT]("cudaStream_t")

  def cudaStreamDefault: Rep[Int] = cmacro[Int]("cudaStreamDefault")
  def cudaStreamNonBlocking: Rep[Int] = cmacro[Int]("cudaStreamNonBlocking")

  // __host__ ​ __device__ ​cudaError_t cudaStreamCreateWithFlags ( cudaStream_t* pStream, unsigned int  flags )
  def cudaStreamCreateWithFlags(stream: Rep[cudaStreamT], flag: Rep[Int]) =
    libFunction[CudaErrorT]("cudaStreamCreateWithFlags", Unwrap(stream), Unwrap(flag))(Seq(0), Seq(0), Set(0))

  // __host__​cudaError_t cudaStreamCreate ( cudaStream_t* pStream )
  def cudaStreamCreate(stream: Rep[cudaStreamT]) =
    libFunction[CudaErrorT]("cudaStreamCreate", Unwrap(stream))(Seq(0), Seq(0), Set(0))

  // __host__ ​cudaError_t cudaStreamSynchronize ( cudaStream_t stream ) // Waits for stream tasks to complete.
  def cudaStreamSynchronize(stream: Rep[cudaStreamT]) =
    libFunction[CudaErrorT]("cudaStreamSynchronize", Unwrap(stream))(Seq(0), Seq(0), Set[Int]())


  // cudaGetDeviceCount(&deviceCount)
  def cudaGetDeviceCount(count: Var[Int]) =
    libFunction[CudaErrorT]("cudaGetDeviceCount", UnwrapV(count))(Seq(), Seq(0), Set(0))

  // cudaError_t cudaMemset( void* devPtr, int value, size_t count )
  def cudaMemset[T:Manifest](devPtr: Rep[Array[T]], value: Rep[Int], count: Rep[SizeT]) =
    libFunction[CudaErrorT]("cudaMemset", Unwrap(devPtr), Unwrap(value), Unwrap(count))(Seq(1,2), Seq(0), Set())
  def cudaMemset2[T:Manifest](devPtr: Rep[Array[T]], value: Rep[Int], count: Rep[Int])(implicit __pos: SourceContext) = {
    cudaMemset[T](devPtr, value, SizeT(count * sizeOf[T]))
  }

  // expf - single precision (float) exponential function
  def expf[N:Numeric:Manifest](x: Rep[N]) =
    libFunction[N]("expf", Unwrap(x))(Seq(), Seq(), Set())

  def log[N:Numeric:Manifest](x: Rep[N]) =
    libFunction[N]("log", Unwrap(x))(Seq(), Seq(), Set())

  class cudaEventT
  def cudaEvent: Rep[cudaEventT] = newStruct[cudaEventT]("cudaEvent_t")

  // __host__​cudaError_t cudaEventCreate( cudaEvent_t* event )
  def cudaEventCreate(event: Rep[cudaEventT]) =
    libFunction[CudaErrorT]("cudaEventCreate", Unwrap(event))(Seq(0), Seq(0), Set(0))

  // __host__​cudaError_t cudaEventDestroy( cudaEvent_t* event )
  def cudaEventDestroy(event: Rep[cudaEventT]) =
    libFunction[CudaErrorT]("cudaEventDestroy", Unwrap(event))(Seq(0), Seq(0), Set())

  // __host__​__device__​cudaError_t 	cudaEventRecord ( cudaEvent_t event, cudaStream_t stream = 0 )
  def cudaEventRecord(event: Rep[cudaEventT], stream: Rep[cudaStreamT]) =
    libFunction[CudaErrorT]("cudaEventRecord", Unwrap(event), Unwrap(stream))(Seq(0, 1), Seq(0, 1), Set())

  // __host__​__device__​cudaError_t 	cudaEventRecord ( cudaEvent_t event, cudaStream_t stream = 0 )
  def cudaEventRecord(event: Rep[cudaEventT]) = {
    libFunction[CudaErrorT]("cudaEventRecord", Unwrap(event))(Seq(0), Seq(0), Set())
  }

  // __host__​__device__cudaError_t cudaEventSynchronize ( cudaEvent_t event )
  def cudaEventSynchronize(event: Rep[cudaEventT]) =
    libFunction[CudaErrorT]("cudaEventSynchronize", Unwrap(event))(Seq(0), Seq(0), Set())

  // __host__​cudaError_t cudaEventElapsedTime ( float* ms, cudaEvent_t start, cudaEvent_t end )
  def cudaEventElapsedTime(ms: Var[Float], start: Rep[cudaEventT], end: Rep[cudaEventT]) = {
    libFunction[CudaErrorT]("cudaEventElapsedTime", UnwrapV(ms), Unwrap(start), Unwrap(end))(Seq(1,2), Seq(0), Set(0))
  }

  // perform time measurement using cudaEvent functions, returns measured execution time
  def measurement_cuda(clo: => Unit)(implicit __pos: SourceContext): Rep[Float] = {
    val start = cudaEvent
    val stop = cudaEvent
    cudaCall(cudaEventCreate(start))
    cudaCall(cudaEventCreate(stop))
    cudaCall(cudaEventRecord(start))
    val exec = clo
    cudaCall(cudaEventRecord(stop))
    cudaCall(cudaEventSynchronize(stop))
    val time = var_new[Float](0.0f)
    cudaCall(cudaEventElapsedTime(time, start, stop))
    val res = time
    res
  }


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

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]) =
    Wrap[(A,B,Dim3,Dim3)=>C](__topFun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)))), "__global__"))

  def cudaGlobalDynamicFun[A:Manifest,B:Manifest,C:Manifest](f: (Rep[A], Rep[B]) => Rep[C]) =
    Wrap[(A,B,Dim3,Dim3,Int)=>C](__topFun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]) =
    Wrap[(A,B,C,Dim3,Dim3)=>D](__topFun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)))), "__global__"))

  def cudaGlobalDynamicFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](f: (Rep[A], Rep[B], Rep[C]) => Rep[D]) =
    Wrap[(A,B,C,Dim3,Dim3,Int)=>D](__topFun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]) =
    Wrap[(A,B,C,D,Dim3,Dim3)=>E](__topFun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)))), "__global__"))

  def cudaGlobalDynamicFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]) =
    Wrap[(A,B,C,D,Dim3,Dim3,Int)=>E](__topFun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)))), "__global__"))
  
  def cudaGlobalDynamicFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F]) => Rep[G]) =
    Wrap[(A,B,C,D,E,F,Dim3,Dim3,Int)=>G](__topFun(f, 6, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => Rep[F]) =
    Wrap[(A,B,C,D,E,Dim3,Dim3)=>F](__topFun(f, 5, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F]) => Rep[G]) =
    Wrap[(A,B,C,D,E,F,Dim3,Dim3)=>G](__topFun(f, 6, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G]) => Rep[H]) =
    Wrap[(A,B,C,D,E,F,G,Dim3,Dim3)=>H](__topFun(f, 7, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H]) => Rep[I]) =
    Wrap[(A,B,C,D,E,F,G,H,Dim3,Dim3)=>I](__topFun(f, 8, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I]) => Rep[J]) =
    Wrap[(A,B,C,D,E,F,G,H,I,Dim3,Dim3)=>J](__topFun(f, 9, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)), Wrap[I](xn(8)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest,K:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I], Rep[J]) => Rep[K]) =
    Wrap[(A,B,C,D,E,F,G,H,I,J,Dim3,Dim3)=>K](__topFun(f, 10, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)), Wrap[I](xn(8)), Wrap[J](xn(9)))), "__global__"))

  def cudaGlobalFun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest,K:Manifest,L:Manifest](f: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I], Rep[J], Rep[K]) => Rep[L]) =
    Wrap[(A,B,C,D,E,F,G,H,I,J,K,Dim3,Dim3)=>L](__topFun(f, 11, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)), Wrap[I](xn(8)), Wrap[J](xn(9)), Wrap[K](xn(10)))), "__global__"))

  // When coding kernel functions, we often need some kernel variables
  def gridDimX: Rep[Int] = cmacro[Int]("gridDim.x")
  def gridDimY: Rep[Int] = cmacro[Int]("gridDim.y")
  def gridDimZ: Rep[Int] = cmacro[Int]("gridDim.z")
  def blockDimX: Rep[Int] = cmacro[Int]("blockDim.x")
  def blockDimY: Rep[Int] = cmacro[Int]("blockDim.y")
  def blockDimZ: Rep[Int] = cmacro[Int]("blockDim.z")

  def blockIdxX: Rep[Int] = cmacro[Int]("blockIdx.x")
  def blockIdxY: Rep[Int] = cmacro[Int]("blockIdx.y")
  def blockIdxZ: Rep[Int] = cmacro[Int]("blockIdx.z")
  def threadIdxX: Rep[Int] = cmacro[Int]("threadIdx.x")
  def threadIdxY: Rep[Int] = cmacro[Int]("threadIdx.y")
  def threadIdxZ: Rep[Int] = cmacro[Int]("threadIdx.z")

  // Here we will implement some cuda kernel functions using the `cudaGlobalFun`
  def cudaFill[T:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (data: Rep[Array[T]], value: Rep[T], size: Rep[Int]) =>
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- tid.until(size, stride): Rep[Range]) {
        data(i) = value
      }
    }

  // cudaCap: cap the absolute value of `data`
  def cudaCap[T:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (data: Rep[Array[T]], bound: Rep[T], size: Rep[Int]) =>
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- tid.until(size, stride): Rep[Range]) {
        __ifThenElse(data(i) > bound, {data(i) = bound}, {})
        __ifThenElse(data(i) < -bound, {data(i) = -bound}, {})
      }
    }

  // hardTanh: cap the value of `data` by `min` and `max`
  def handTanh[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (data: Rep[Array[N]], out: Rep[Array[N]], min: Rep[N], max: Rep[N], size: Rep[Int]) =>
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- tid.until(size, stride): Rep[Range]) {
        out(i) = __ifThenElse(data(i) < min, min,
                 __ifThenElse(data(i) > max, max, data(i)))
      }
    }

  // hardTanhGrad: backward of hardTanh, with `inPlace` option
  def hardTanhGrad[N:Numeric:Manifest](inPlace: Boolean)(implicit __pos: SourceContext) = cudaGlobalFun {
    (inX: Rep[Array[N]], inD: Rep[Array[N]], outD: Rep[Array[N]], minVal: Rep[N], maxVal: Rep[N], size: Rep[Int]) =>
      val stride = gridDimX * blockDimX
      val tid = threadIdxX + blockIdxX * blockDimX
      for (i <- tid.until(size, stride): Rep[Range]) {
        if (inPlace) {
          __ifThenElse(inX(i) < minVal || inX(i) > maxVal, {inD(i) = unit(implicitly[Numeric[N]].zero)}, {})
        } else {
          __ifThenElse(inX(i) >= minVal && inX(i) <= maxVal, {inD(i) += outD(i)}, {})
        }
      }
    }

  // nllLoss: x: prediction with probabiity, x_stride: feature_size of the 2D x
  //          y: result of nllLoss, label: int-type, true label
  def nllLoss[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (x: Rep[Array[N]], xStride: Rep[Int], y: Rep[Array[N]], label: Rep[Array[Int]]) =>
      // Note: each tid is for a sample in a batch
      // xStride is the number of features
      val tid = threadIdxX + blockIdxX * blockDimX
      val offset = tid * xStride + label(tid)
      y(tid) = -x(offset)
    }

  // nllLoss_grad:
  def nllLossGrad[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (xGrad: Rep[Array[N]], xStride: Rep[Int], yGrad: Rep[Array[N]], label: Rep[Array[Int]]) =>
      // Note: each tid is for a sample in a batch
      val tid = threadIdxX + blockIdxX * blockDimX
      val offset = tid * xStride + label(tid)
      xGrad(offset) -= yGrad(tid)
    }

  // sumGrad: grad operation of a sum op (by a given axis)
  // FIXME(feiw) to be tested
  def sumGrad[N:Numeric:Manifest](rank: Int, dim: Int)(implicit __pos: SourceContext) = cudaGlobalFun {
    (xGrad: Rep[Array[N]], xSize: Rep[Array[Int]], size: Rep[Int], yGrad: Rep[Array[N]], yStrides: Rep[Array[Int]]) =>
      val tid = threadIdxX + blockIdxX * blockDimX
      val stride = gridDimX * blockDimX
      for (i <- tid.until(size, stride): Rep[Range]) {
        // compute indice of yGrad from flat tid (i)
        val indices = scala.collection.mutable.ArrayBuffer[Rep[Int]]()
        var offset = i
        for (j <- ((0 until rank): Range).reverse) {
          val tempOffset = offset / xSize(j)
          if (j != dim)
            indices.prepend(offset - tempOffset * xSize(j))
          offset = tempOffset
        }
        // compute offset of yGrad from indice
        val yOffset = indices.zipWithIndex.foldLeft(unit(0)) {
          case (o, (d, i)) => o + d * yStrides(i)
        }
        // operate data
        xGrad(i) += yGrad(yOffset)
      }
    }


  // saxpy: single-precision a * x + y
  def saxpy[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (size: Rep[Int], a: Rep[N], x: Rep[Array[N]], y: Rep[Array[N]]) =>
      generate_comment("this is cuda saxpy (single-precision A * X plus Y) kernel")
      generate_comment("arg0: size of input array")
      val i = blockIdxX * blockDimX + threadIdxX
      __ifThenElse(i < size, {y(i) = a * x(i) + y(i)}, {})
  }
}

trait CudaLibs extends CudaOps {
  def cudaEmbedding[T:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (embedding: Rep[Array[T]], indices: Rep[Array[Int]], output: Rep[Array[T]], embed_size: Rep[Int]) => {
      generate_comment("this is cuda embedding kernel.")
      generate_comment("arg0: 2D embedding table: <n_embedding x embed_size>")
      generate_comment("arg1: 1D indices: <indices_size>")
      generate_comment("arg2: 2D output: <indices_size x embed_size>")
      generate_comment("arg3: embed_size")
      generate_comment("invocation assumption: <<<dim3(a,1,1), dim3(indices_size,1,1)>>> where a <= embed_size")
      generate_comment("each thread block handles one embedding vector")
      val posIdx = indices(blockIdxX)
      val tid = threadIdxX
      val stride = blockDimX
      for (i <- tid.until(embed_size, stride): Rep[Range]) {
        output(blockIdxX * embed_size + i) = embedding(posIdx * embed_size + i)
      }
    }
  }

  def cudaEmbeddingGrad[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalDynamicFun {
    (indicies: Rep[Array[Int]], grad: Rep[Array[N]], gradWeight: Rep[Array[N]], n: Rep[Int], stride: Rep[Int], paddingIdx: Rep[Int]) => {
      generate_comment("Cuda Embedding Grad")
      generate_comment("arg0: embedding indicies")
      generate_comment("arg1: embedding output gradient")
      generate_comment("arg2: embedding gradient")
      generate_comment("arg3: indicies size")
      generate_comment("arg4: padding index (-1 if unsure)")
      val buffer = NewDynSharedArray[N]
      val my_s = buffer.slice(warpSize * threadIdxY, warpSize * threadIdxY)
      val indicies_batch = CastArray[N, Int](buffer.slice(warpSize * blockDimY, warpSize * blockDimY))
      val feature_dim = threadIdxX + blockIdxX * blockDimX

      def conditional_assign[T:Numeric:Manifest](cond: Rep[Boolean], dst: Rep[Array[T]], in: Rep[Int], src: Rep[Array[T]], out: Rep[Int]) = {
        __ifThenElse(cond, {dst(in) = src(out)}, {})
      }

      def min[T:Numeric:Manifest](x: Rep[T], y: Rep[T]) = {
        __ifThenElse(x < y, x, y)
      }

      for (batch_start <- (0 until (n, blockDimX * blockDimY)): Rep[Range]) {
        // Entire block cooperates to load a batch of 1024 indices to process
        val tid = threadIdxX + threadIdxY * blockDimX
        conditional_assign(batch_start + tid < n, indicies_batch, tid, indicies, batch_start + tid)
        val batch_end = min(batch_start + blockDimX * blockDimY, n)

        // Loop over the batch of <= 1024 loaded indices in chunks of blockDim.y = 32
        for (chunk_start <- (batch_start until (batch_end, blockDimY)): Rep[Range]) {
          // Sync to make sure that indicies_batch is ready and to ensure that
          // match-group leaders are done with their accumulates before other warps
          // start loading again
          cudaSyncThreads

          val n_this_chunk = min(batch_end - chunk_start, blockDimY)
          val src_row = chunk_start + threadIdxY
          val dst_row = indicies_batch(src_row - batch_start)
          conditional_assign((src_row < n) && (feature_dim < stride) && notequals(dst_row, paddingIdx), my_s, threadIdxX, grad, src_row * stride + feature_dim)

          cudaSyncThreads
          
          // To ensure determinism, we can't just have each warp add its grad data to its dst_row.
          // We need to check if any other warps pulled grad data targeting dst_row.
          // If so, we elect the first warp in each matching group as the leader.
          // Each leader warp serializes the accumulates targeting dst_row in shared memory,
          // then finishes by adding the accumulated buffer to dst_row in grad_weight.
          __ifThenElse(src_row < n && notequals(dst_row, paddingIdx), {
            val match_found_this_thread = var_new(__ifThenElse(equals(dst_row, indicies_batch(chunk_start - batch_start + threadIdxX)), 1, 0))
            __ifThenElse(threadIdxX >= n_this_chunk, {__assign(match_found_this_thread, 0)}, {})

            val matchmask = var_new(cudaBallotSync(0xffffffff, match_found_this_thread))
            indicies(n_this_chunk) = matchmask 
            val first_remaining_peer = var_new(cudaFfs(matchmask) - 1)

            __ifThenElse(equals(threadIdxY, readVar(first_remaining_peer)), {
              __assign(matchmask, matchmask ^ (1 << readVar(first_remaining_peer)))
              __whileDo(readVar(matchmask) > 0, {
                __assign(first_remaining_peer, cudaFfs(matchmask) - 1)
                my_s(threadIdxX) = my_s(threadIdxX) + buffer(threadIdxX + warpSize * first_remaining_peer)
                __assign(matchmask, matchmask ^ (1 << readVar(first_remaining_peer)))
              })

              __ifThenElse(feature_dim < stride, {gradWeight(dst_row * stride + feature_dim) += my_s(threadIdxX)}, {})
              ()
            }, {})
          }, {})
        }
      }
    }
  }

  def cudaMaskedFill[N:Numeric:Manifest](ijSwapped: Boolean)(implicit __pos: SourceContext) = cudaGlobalFun {
    (in: Rep[Array[N]], out: Rep[Array[N]], mask: Rep[Array[Int]], value: Rep[N],
    dim0_shape: Rep[Int], dim1_shape: Rep[Int], dim0_stride: Rep[Int], dim1_stride: Rep[Int],
    input_size: Rep[Int]) => {
      generate_comment("this is the cuda masked fill kernel.")
      generate_comment("The kernel takes an N-d input tensor `in` and selects two dimensions `i` and `j` to work with.")
      generate_comment("`ijSwapped` is true if and only if i > j.")
      generate_comment("`dim0_shape` and `dim1_shape` are shapes of dimension `i` and `j` in input tensor, respectively.")
      generate_comment("`dim0_stide` and `dim1_stide` denote the physical distance between two logically contigent elements")
      generate_comment("in `i` and `j` of the input array, respectively.")
      generate_comment("The kernel also takes a 2-d `mask` tensor, of shape (dim0_shape, dim1_shape). This mask tensor")
      generate_comment("contains only zeros and ones. The kernel fills elements of input tensor with `value` where mask is")
      generate_comment("zero and stores the result to `out`.")

      val tid = var_new[Int](blockIdxX * blockDimX + threadIdxX)
      val stride = blockDimX * gridDimX

      val i = var_new[Int](tid / dim0_stride)
      val j = var_new[Int]((tid - i * dim0_stride) / dim1_stride)
      val inner_idx = var_new[Int](tid - i * dim0_stride - j * dim1_stride)
      val idx = var_new[Int](i * dim0_stride + j * dim1_stride + inner_idx)

      __whileDo(idx < input_size, {
        val mask_id = if (ijSwapped)
          (j % dim1_shape) * dim0_shape + (i % dim0_shape) else
          (i % dim0_shape) * dim1_shape + (j % dim1_shape)

        out(idx) = __ifThenElse(ordering_equiv(mask(mask_id), 0), { in(idx) }, { value })

        __assign(tid, tid + stride)
        __assign(i, tid / dim0_stride)
        __assign(j, (tid - i * dim0_stride) / dim1_stride)
        __assign(inner_idx, tid - i * dim0_stride - j * dim1_stride)
        __assign(idx, i * dim0_stride + j * dim1_stride + inner_idx)

      })
    }
  }

  def cudaMaskedFillWrap[N:Numeric:Manifest](in: Rep[Array[N]], out: Rep[Array[N]], mask: Rep[Array[Int]], shape: Seq[Int], size: Int, value: N)(implicit __pos: SourceContext) = {
    // choose the last two dimensions as dim0 and dim1
    val dim0_shape = shape(shape.size - 2)
    val dim1_shape = shape(shape.size - 1)
    val dim0_stride = dim0_shape
    val dim1_stride = 1

    val grid = dim3((size + 511) / 512)
    val block = dim3(512)

    val kernel = cudaMaskedFill[N](false)
    kernel(in, out, mask, value, dim0_shape, dim1_shape, dim0_stride, dim1_stride, size, grid, block)
  }

  def cudaMaskedFillGrad[N:Numeric:Manifest](ijSwapped: Boolean)(implicit __pos: SourceContext) = cudaGlobalFun {
    (y_d: Rep[Array[N]], x_d: Rep[Array[N]], mask: Rep[Array[Int]],
    dim0_shape: Rep[Int], dim1_shape: Rep[Int], dim0_stride: Rep[Int], dim1_stride: Rep[Int],
    input_size: Rep[Int]) => {
      generate_comment("this is the cuda masked fill gradient kernel.")
      generate_comment("arg0: gradient of N-d output tensor.")
      generate_comment("arg1: gradient of N-d input tensor.")
      generate_comment("Other parameters are same as maskedFill")

      val tid = var_new[Int](blockIdxX * blockDimX + threadIdxX)
      val stride = blockDimX * gridDimX

      val i = var_new[Int](tid / dim0_stride)
      val j = var_new[Int]((tid - i * dim0_stride) / dim1_stride)
      val inner_idx = var_new[Int](tid - i * dim0_stride - j * dim1_stride)
      val idx = var_new[Int](i * dim0_stride + j * dim1_stride + inner_idx)

      __whileDo(idx < input_size, {
        val mask_id = if (ijSwapped)
          (j % dim1_shape) * dim0_shape + (i % dim0_shape) else
          (i % dim0_shape) * dim1_shape + (j % dim1_shape)

        __ifThenElse(ordering_equiv(mask(mask_id), 0), { x_d(idx) = x_d(idx) + y_d(idx) }, {  })

        __assign(tid, tid + stride)
        __assign(i, tid / dim0_stride)
        __assign(j, (tid - i * dim0_stride) / dim1_stride)
        __assign(inner_idx, tid - i * dim0_stride - j * dim1_stride)
        __assign(idx, i * dim0_stride + j * dim1_stride + inner_idx)
      })
    }
  }

  def cudaMaskedFillGradWrap[N:Numeric:Manifest](dout: Rep[Array[N]], din: Rep[Array[N]], mask: Rep[Array[Int]], shape: Seq[Int], size: Int)(implicit __pos: SourceContext) = {
    // choose the last two dimensions as dim0 and dim1
    val dim0_shape = shape(shape.size - 2)
    val dim1_shape = shape(shape.size - 1)
    val dim0_stride = dim0_shape
    val dim1_stride = 1

    val grid = dim3((size + 511) / 512)
    val block = dim3(512)
    
    val kernel = cudaMaskedFillGrad[N](false)
    kernel(dout, din, mask, dim0_shape, dim1_shape, dim0_stride, dim1_stride, size, grid, block)
  }

  // Cuda Tranpose Values
  val blockRows = 8
  val tileDim = 32

  // cuda matrix copy function for baseline
  def cudaMatrixCopy[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (in: Rep[Array[N]], out: Rep[Array[N]]) =>
      generate_comment("Cuda Matrix Copy")
      (generate_comment("arg0: 2D Input Matrix (n x n) where n is a multiple of 32"))
      (generate_comment("arg1: 2D Output Matrix (n x n) where n is a multiple of 32"))

      val x = blockIdxX * tileDim + threadIdxX
      val y = blockIdxY * tileDim + threadIdxY
      val width = gridDimX * tileDim

      for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
        out((y + i) * width + x) = in((y + i) * width + x)
      }
  }

  // naive cuda transpose kernel
  def cudaTransposeNaive[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (in: Rep[Array[N]], out: Rep[Array[N]]) =>
      generate_comment("Cuda Transpose Naive")
      (generate_comment("arg0: 2D Input Matrix (n x n) where n is a multiple of 32"))
      (generate_comment("arg1: 2D Output Matrix (n x n) where n is a multiple of 32"))

      val x = blockIdxX * tileDim + threadIdxX
      val y = blockIdxY * tileDim + threadIdxY
      val width = gridDimX * tileDim

      for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
        out(x * width + (y + i)) = in((y + i) * width + x)
      }
  }

  // cuda 2D transpose using coalesced approach
  def cudaTranspose[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (in: Rep[Array[N]], out: Rep[Array[N]], n: Rep[Int], m: Rep[Int]) =>
      generate_comment("Cuda Coalesced Transpose")
      generate_comment("arg0: 2D Input Matrix (n x m)")
      generate_comment("arg1: 2D Output Transposed Matrix (m x n)")
      generate_comment("arg2: number of rows for input matrix")
      generate_comment("arg3: number of columns for input matrix")
      generate_comment("kernel launch config <<dim3((TILE_DIM * m - 1) / TILE_DIM, (TILE_DIM * n - 1) / TILE_DIM), dim3(TILE_DIM, BLOCK_ROWS)>>")
      generate_comment("TILE_DIM = 32, BLOCK_ROWS = 8")

      val tile = NewSharedArray[N](tileDim, tileDim + 1)
      val x = var_new(blockIdxX * tileDim + threadIdxX)
      val y = var_new(blockIdxY * tileDim + threadIdxY)

      for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
        __ifThenElse((x < m) && (y < n), {
          tile(threadIdxY + i, threadIdxX) = in(y * m + x)
        }, {})

        __assign(y, y + blockRows)
      }

      cudaSyncThreads

      __assign(x, blockIdxY * tileDim + threadIdxX)
      __assign(y, blockIdxX * tileDim + threadIdxY)

      for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
        __ifThenElse(x < n && y < m, {
          out(y * n + x) = tile(threadIdxX, threadIdxY + i)
        }, {})
        __assign(y, y + blockRows)
      }
    }


  /*
    reduce the data input array (from 0 to size) using the given op.
    buffer(0) will contain the output of the reduction
   */
  def reduceHelper[N:Numeric:Manifest](input: Rep[Array[N]], size: Rep[Int], buffer: Rep[Array[N]], z: Rep[N], op: (Rep[N], Rep[N]) => Rep[N], skipFirstReduce:Boolean = false)(implicit  __pos: SourceContext) = {
    val start = threadIdxX
    val end = size
    val stride = blockDimX

    if (!skipFirstReduce) {
      generate_comment("thread local reduce")
      val threadVal = var_new[N](z)

      for(i <- start.until(end, stride): Rep[Range]) {
        __assign(threadVal, op(threadVal, input(i)))
      }
      buffer(threadIdxX) = threadVal
      cudaSyncThreads
    }

    generate_comment("reduce to the first warp")
    val warpVal = var_new[N](z)
    __ifThenElse(threadIdxX < blockDimX / warpSize, {
      val lane = threadIdxX
      // TODO(Supun): need #pragma unroll here
      for(i <- 0 until warpSize: Rep[Range]) {
        __assign(warpVal, op(warpVal, buffer(lane * warpSize + i)))
      }
      buffer(lane) = warpVal
    }, {})
    cudaSyncThreads

    generate_comment("reduce to the first thread")

    __ifThenElse(ordering_equiv(threadIdxX, 0), {
      val localVal = var_new[N](z)
      for(i <- 0 until blockDimX / warpSize: Rep[Range]) {
        __assign(localVal, op(localVal, buffer(i)))
      }
      buffer(0) = localVal
    }, {})

    cudaSyncThreads
  }

  def cudaSoftmax[N:Numeric:Manifest](logSoftmax: Boolean)(implicit __pos: SourceContext) = cudaGlobalDynamicFun {
    (input: Rep[Array[N]], output: Rep[Array[N]], lastDimSize: Rep[Int]) =>
      generate_comment("This is cuda softmax (for larger; >=1024 inputs). Performs softmax on last dim.")
      generate_comment("arg0: input: <outerSize x lastDimSize>")
      generate_comment("arg1: output: <outerSize x lastDimSize>")
      generate_comment("arg2: lastDimSize: size of the last dimension (i.e., the softmax dim)")
      generate_comment("invocation assumption: <<<dim3(outerSize,1,1), dim3(1024,1,1), 1024*4>>>")
      val buffer = NewDynSharedArray[N]

      val input_t = input.slice(lastDimSize * blockIdxX, lastDimSize * blockIdxX + lastDimSize)
      val output_t = output.slice(lastDimSize * blockIdxX, lastDimSize * blockIdxX + lastDimSize)

      // find the max
      reduceHelper[N](input_t, lastDimSize, buffer, -infinity[N], (a: Rep[N], b: Rep[N]) => __ifThenElse(a < b, b, a))

      val localVal = var_new[N](implicitly[Numeric[N]].zero)
      for(i <- threadIdxX.until(lastDimSize, blockDimX)) {
        val normalized = input_t(i) - buffer(0)
        val expVal = expf[N](normalized)
        localVal += expVal
        if (logSoftmax) {
          output_t(i) = normalized
        } else {
          output_t(i) = expVal
        }
      }

      buffer(threadIdxX) = localVal
      cudaSyncThreads

      // find the sum
      // thread level reduce is already done, skip the first reduce
      reduceHelper[N](input_t, lastDimSize, buffer, implicitly[Numeric[N]].zero, (a: Rep[N], b: Rep[N]) => a+b, skipFirstReduce = true)

      for(i <- threadIdxX.until(lastDimSize, blockDimX)) {
        if (logSoftmax) {
          output_t(i) = output_t(i) - log[N](buffer(0))
        } else {
          output_t(i) = output_t(i) / buffer(0)
        }
      }
  }

  def cudaSoftmaxGrad[N:Numeric:Manifest](logSoftmax: Boolean)(implicit __pos: SourceContext) = cudaGlobalDynamicFun {
    (gradInput: Rep[Array[N]], gradOutput: Rep[Array[N]], output: Rep[Array[N]], size: Rep[Int]) =>
      generate_comment("This is cuda softmax (for larger; >=1024 inputs). Performs softmax on last dim.")
      generate_comment("arg0: gradInput: the gradient of the original input (i.e., the softmax input) - " +
        "This is an output of the kernel")
      generate_comment("arg1: gradOutput: gradient of softmax output, coming from upstream; An Input to the kernel")
      generate_comment("arg2: output: output of softmax forward pass")
      generate_comment("arg3: size: last dimension size")
      generate_comment("invocation assumption: <<<dim3(outerSize,1,1), dim3(1024,1,1), 1024*4>>>")

      val buffer = NewDynSharedArray[N]

      val gradInput_t = gradInput.slice(size * blockIdxX, size * blockIdxX + size)
      val gradOutput_t = gradOutput.slice(size * blockIdxX, size * blockIdxX + size)
      val output_t = output.slice(size * blockIdxX, size * blockIdxX + size)

      val start = threadIdxX
      val end = size
      val stride = blockDimX

      // compute the sum (gradOutput * output sum)
      val threadVal = var_new[N](implicitly[Numeric[N]].zero)
      for(i <- start.until(end, stride)) {
        if (logSoftmax) {
          threadVal += gradOutput_t(i)
        } else {
          threadVal += gradOutput_t(i) * output_t(i)
        }
      }
      buffer(threadIdxX) = threadVal

      cudaSyncThreads
      reduceHelper[N](null, size, buffer, implicitly[Numeric[N]].zero, (a: Rep[N], b: Rep[N]) => a+b, skipFirstReduce = true)

      // update the gradient
      for(i <- start.until(end, stride)) {
        if (logSoftmax) {
          gradInput_t(i) = gradOutput_t(i) - buffer(0) * expf[N](output_t(i))
        } else {
          gradInput_t(i) = output_t(i) * (gradOutput_t(i) - buffer(0))
        }
      }
  }

  // This is the kernel function for splitting 3D input at the inner most axis.
  // the input is of shape (dimZ, dimY, sum(dimXs))
  // the number of outputs is determined by the dimXs, which is a list of outputs' dimX
  def cuda3DSplitAxis2[N:Numeric:Manifest](dimZ: Int, dimY: Int, dimXs: List[Int])(implicit __pos: SourceContext) = cudaGlobalFun {
    val dimX = dimXs.reduce(_ + _)
    val input_size = dimZ * dimY * dimX
    val offsets = dimXs.scanLeft(0) { case (acc, x) => acc + x }
    val n = dimXs.length

    (in: Rep[Array[N]], out: Rep[Array[Array[N]]]) => {
      generate_comment(s"This is cuda $n-section split kernel for 3D input at axis 2.")
      generate_comment(s"It takes a 3D array and splits on the innermost dimension (dim2) into $n arrays.")
      generate_comment("arg0: input array")
      generate_comment("arg1: array of output arrays")
      generate_comment(s"call constraint: sum of out(i).size = in.size for i in [0, $n)")

      val idx = blockIdxX * blockDimX + threadIdxX

      __ifThenElse(idx < input_size, {
        val value = in(idx)
        val x = idx / dimX
        val y = idx % dimX

        def recursive_branch(t: Int): Unit =
          if (t == n-1) {
            write_array_element(t)
          } else {
            __ifThenElse(y < offsets(t+1), { write_array_element(t) }, { recursive_branch(t+1) })
          }

        def write_array_element(t: Int) = {
          val temp = out(t)
          cudaArrayEffect[N](temp, x * dimXs(t) + (y - offsets(t)), value)
        }

        recursive_branch(0)
      }, {})
    }
  }

  def cuda3DSplitWrap[N:Numeric:Manifest](in: Rep[Array[N]], outs: List[Rep[Array[N]]], dimZ: Int, dimY: Int, dimXs: List[Int])(implicit __pos: SourceContext) = {
    val sec = outs.length
    val output = NewArray[Array[N]](sec)
    for (i <- (0 until sec): Range) {
      output(i) = outs(i)
    }
    val cuda_output = cudaMalloc2[Array[N]](sec)
    cudaCall(cudaMemcpyOfT[Array[N]](cuda_output, output, sec, host2device))

    val in_sz = dimZ * dimY * dimXs.reduce(_ + _)
    val grid = dim3((in_sz + 511)/512)
    val block = dim3(512)

    val splitKernel = cuda3DSplitAxis2[N](dimZ, dimY, dimXs)
    splitKernel(in, cuda_output, grid, block)
  }

  // This is the kernel function for concatenating 3D inputs at the inner most axis
  // the input i is of shape (dimZ, dimY, dimXs(i))
  // the output shape is (dimZ, dimY, sum(dimXs))
  def cuda3DConcatAxis2[N:Numeric:Manifest](dimZ: Int, dimY: Int, dimXs: List[Int])(implicit __pos: SourceContext) = cudaGlobalFun {
    val dimX = dimXs.reduce(_ + _)
    val d_other = dimZ * dimY
    val input_size = d_other * dimX
    val offsets = dimXs.scanLeft(0) { case (acc, x) => acc + x }
    val n = dimXs.length

    (in: Rep[Array[Array[N]]], out: Rep[Array[N]]) => {
      generate_comment(s"this is cuda $n-section concat kernel for 3D inputs at axis 2.")
      generate_comment(s"It concatenates $n 3D arrays on the innermost dimension (dim2).")
      generate_comment("arg0: array of input input arrays")
      generate_comment("arg1: output array")
      generate_comment(s"call constraint: in.size = $n")
      generate_comment(s"call constraint: sum of in(i).size = out.size for i in [0, $n)")

      val idx = blockIdxX * blockDimX + threadIdxX

      __ifThenElse(idx < input_size, {
        val x = idx / dimX
        val y = idx % dimX

        def recursive_branch(t: Int): Unit =
          if (t == n-1) {
            write_array_element(t)
          } else {
            __ifThenElse(y < offsets(t+1), { write_array_element(t) }, { recursive_branch(t+1) })
          }

        def write_array_element(t: Int) = {
          val arr = in(t)
          out(idx) = arr(x * dimXs(t) + (y - offsets(t)))
        }

        recursive_branch(0)
      }, {})
    }
  }

  def cuda3DConcatWrap[N:Numeric:Manifest](ins: List[Rep[Array[N]]], out: Rep[Array[N]], dimZ: Int, dimY: Int, dimXs: List[Int])(implicit __pos: SourceContext) = {
    val sec = ins.length
    val input = NewArray[Array[N]](sec)
    for (i <- (0 until sec): Range) {
      input(i) = ins(i)
    }
    val cuda_input = cudaMalloc2[Array[N]](sec)
    cudaCall(cudaMemcpyOfT[Array[N]](cuda_input, input, sec, host2device))

    val out_sz = dimXs.reduce(_ + _) * dimY * dimZ
    val grid = dim3((out_sz + 511)/512)
    val block = dim3(512)

    val concatKernel = cuda3DConcatAxis2[N](dimZ: Int, dimY: Int, dimXs: List[Int])
    concatKernel(cuda_input, out, grid, block)
  }

  // kernel function for 2D transpose
  def cudaTranspose2[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (in: Rep[Array[N]], out: Rep[Array[N]], dimY: Rep[Int], dimX: Rep[Int]) =>
      generate_comment("this is the transpose kernel")
      generate_comment("arg0: 2D Input Matrix (dimY x dimX) may not be multiples of 32")
      generate_comment("arg1: 2D Output Matrix (dimX x dimY)")
      generate_comment("arg2: dimY of input")
      generate_comment("arg3: dimX of input")
      generate_comment("caller must use <<<dim3((dimX+31)/32, (dimY+31)/32, 1), dim3(32, 8, 1)>>>")
      generate_comment("using gridDimX=(dimX+31)/32, gridDimY=(dimY+31)/32, blockDimX=32, blockDimY=8")
      val tile = NewSharedArray[N](tileDim, tileDim + 1) // for resolving banking conflict

      generate_comment("read data from input array to shared memory")
      // helper function from block indices and thread indices to input reads
      def in_offset(bX: Rep[Int], bY: Rep[Int], tX: Rep[Int], tY: Rep[Int], func: Rep[N] => Unit) = {
        val x = bX * tileDim + tX
        val y = bY * tileDim + tY
        __ifThenElse(y < dimY && x < dimX, func(in(y * dimX + x)), {})
      }
      for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
        in_offset(blockIdxX, blockIdxY, threadIdxX, threadIdxY + i, value => {
          tile(threadIdxY + i, threadIdxX) = value
        })
      }

      generate_comment("sync threads")
      cudaSyncThreads

      generate_comment("write data from shared memory to output array")
      // helper function from block indices and thread indices to output writes
      def out_offset(bX: Rep[Int], bY: Rep[Int], tX: Rep[Int], tY: Rep[Int], value: Rep[N]) = {
        val x = bX * tileDim + tX
        val y = bY * tileDim + tY
        __ifThenElse(y < dimX && x < dimY, {out(y * dimY + x) = value}, {})
      }
      // we want to swap block indices, but maintain thread indices (so that the writes are coaleased)
      // then we must read the tile in a transposed manner (swap read indices)
      for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
        out_offset(blockIdxY, blockIdxX, threadIdxX, threadIdxY + i, tile(threadIdxX, threadIdxY + i))
      }
  }

  // amazing helper function to get flatten offset of tensors
  def flatten(shape: List[Rep[Int]], index: List[Rep[Int]])(implicit __pos: SourceContext): Rep[Int] = {
    val a: Rep[Int] = (shape.tail zip index.init).foldLeft(unit(0)) {
      case (z: Rep[Int], (shape: Rep[Int], index: Rep[Int])) => (index + z) * shape
    }
    a + index.last
  }

  // kernel function for 3D permute
  def cudaPermute3D[N:Numeric:Manifest](permutation: List[Int])(implicit __pos: SourceContext) = {
    require(permutation.length == 3)
    // we can put all sub-functions of 3D permutation here because the function type is the same

    if (permutation == List(0, 1, 2)) {
      // case 0: permutation is identity
      throw new Exception("identity permutation is not allowed in permutation kernel")
    } else if (permutation == List(1, 0, 2)) {
      // case 1: permutation is not touching the inner most dimension
      cudaPermute102[N]
    } else {
      // case 2: the dimX (inner most dim) is changed to a dim that is not inner most
      //         another dim (dimY or dimZ) is changed to the inner most dim
      cudaGlobalFun {(in: Rep[Array[N]], out: Rep[Array[N]], dimZ: Rep[Int], dimY: Rep[Int], dimX: Rep[Int]) =>

        // need to have 3D grids and 3D blocks
        // the 3D blocks handle a block of size (tileDim, tileDim, 1) where one of the tileDim is for dimX
        //   and the other tileDim is for the dim that becomes the new inner most dim.
        val blockDim = permutation match {
          case List(0,2,1) => List(1, tileDim, tileDim)
          case List(1,2,0) => List(tileDim, 1, tileDim)
          case List(2,1,0) => List(tileDim, 1, tileDim)
          case List(2,0,1) => List(1, tileDim, tileDim)
        }
        val sharedMemSize = permutation match {
          // should be the same size as blockDim
          // except with +1 on the inner most size to avoid bank conflict
          case List(0,2,1) => List(1, tileDim, tileDim + 1)
          case List(1,2,0) => List(tileDim, 1, tileDim + 1)
          case List(2,1,0) => List(tileDim, 1, tileDim + 1)
          case List(2,0,1) => List(1, tileDim, tileDim + 1)
        }
        // the actual block dims is (tileDim, blockRows, 1), where the blockRows is 4 times smaller than tileDim
        // so that each block has a loop of size 4
        val actualBlockDim = permutation match {
          case List(0,2,1) => List(1, blockRows, tileDim)
          case List(1,2,0) => List(blockRows, 1, tileDim)
          case List(2,1,0) => List(blockRows, 1, tileDim)
          case List(2,0,1) => List(1, blockRows, tileDim)
        }

        // block indices:
        val blocks = List(blockIdxZ, blockIdxY, blockIdxX)
        // effective thread indices (+i for the dimension with size blockRows, because it needs to be larger in a loop)
        val threads = (i: Rep[Int]) => permutation match {
          case List(0,2,1) => List(threadIdxZ, threadIdxY + i, threadIdxX)
          case List(1,2,0) => List(threadIdxZ + i, threadIdxY, threadIdxX)
          case List(2,1,0) => List(threadIdxZ + i, threadIdxY, threadIdxX)
          case List(2,0,1) => List(threadIdxZ, threadIdxY + i, threadIdxX)
        }

        // the output block dim is different from the `blockDim`, since the output is permuted
        val outBlockDim = permutation.map(blockDim(_))
        // output block indices: needs to permute block indices
        val outBlocks = permutation.map(blocks(_))
        // output thread indices: needs to permute thread indices
        val outThreads_ = (i: Rep[Int]) => permutation.map(threads(i)(_))

        // helper function what swapping a list with 2 indices
        def swapper[N](l: List[N], i: Int, j: Int) = l.updated(i, l(j)).updated(j, l(i))

        // However, if the last element of outThreads_ is not threadIdxX, the access
        // to output array will be not coaleased. To make it coaleased, we need to swap
        // the workload of `threadIdxX` with whatever `the last element of outThreads_`
        // that is to say, we want to swap `threadIdx` with `last element of outThreads_` in `outThreads`
        // Luckily, the index of `last element of outThreads_` is for sure `2`
        //     and  the index of `threadIdx` in `outThreads_` is `permutation.indexOf(2)`
        val outThreads = (i: Rep[Int]) => swapper(outThreads_(i), 2, permutation.indexOf(2))
        // now we also need to swap the indices when reading from shared memory
        // specifically, we need to swap the `threadIdx` and `last element of outThreads_` in `threads`
        // Luckily, the index of `threadIdx` in `threads` is for sure `2`
        //     and  the index of `last element of outThreads_` in `threads` is just `permutation.last`
        val swappedThreads = (i: Rep[Int]) => swapper(threads(i), permutation.last, 2)

        // for comments
        // shape and output shape
        val shape = List("dimZ", "dimY", "dimX")
        val outShape = permutation.map(shape(_))
        // the blocks
        val blockDimComment = s"dim3(${actualBlockDim(2)}, ${actualBlockDim(1)}, ${actualBlockDim(0)})"
        // the grids should cover the input tensor, module the blocks
        val grids = List("dimZ", "dimY", "dimX").zip(blockDim) map { case (s, b) => if (b == 1) s else s"($s+${b-1})/$b" }
        val gridDimComment = s"dim3(${grids(2)}, ${grids(1)}, ${grids(0)})"

        generate_comment(s"this is the permutation kernel for ${permutation}")
        generate_comment(s"arg0: 3D input tensor (${shape(0)} x ${shape(1)} x ${shape(2)})")
        generate_comment(s"arg1: 3D output tensor (${outShape(0)} x ${outShape(1)} x ${outShape(2)})")
        generate_comment("arg2: dimZ of input")
        generate_comment("arg3: dimY of input")
        generate_comment("arg4: dimX of input")
        generate_comment(s"caller must use <<<$gridDimComment, $blockDimComment>>>")
        val tile = NewSharedArray[N](sharedMemSize(0), sharedMemSize(1), sharedMemSize(2))

        generate_comment("read data from input array to shared memory")
        // helper function from block indices and thread indices to input reads
        def in_offset(blocks: List[Rep[Int]], threads: List[Rep[Int]], func: Rep[N] => Unit) = {
          val indices = (blocks.zip(blockDim)).zip(threads).map {case ((b, bD), t) => b * bD + t }
          val guard = (indices(0) < dimZ) && (indices(1) < dimY) && (indices(2) < dimX)
          val flatten_idx = flatten(List(dimZ, dimY, dimX), indices)
          __ifThenElse(guard, func(in(flatten_idx)), {})
        }
        for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
          in_offset(blocks, threads(i), value => { tile(threads(i)) = value })
        }

        generate_comment("sync threads")
        cudaSyncThreads

        generate_comment("write data from shared memory to output array")
        // helper function from block indices and thread indices to output writes
        def out_offset(blocks: List[Rep[Int]], threads: List[Rep[Int]], value: Rep[N]) = {
          val indices = blocks.zip(outBlockDim).zip(threads).map{ case((b, bD), t) => b * bD + t }
          val originalShape = List(dimZ, dimY, dimX)
          val permutedShape = permutation.map(originalShape(_))
          val guard = indices(0) < permutedShape(0) && indices(1) < permutedShape(1) && indices(2) < permutedShape(2)
          val flatten_idx = flatten(permutedShape, indices)
          __ifThenElse(guard, {out(flatten_idx) = value}, {})
        }
        for (i <- (0 until (tileDim, blockRows)): Rep[Range]) {
          out_offset(outBlocks, outThreads(i), tile(swappedThreads(i)))
        }
      }
    }
  }

  // kernel function for 3D permutation of [1, 0, 2] when dimX is big
  // TODO(feiw) permutation of [1, 0, 2] when dimX is not big!
  def cudaPermute102[N:Numeric:Manifest](implicit __pos: SourceContext) = cudaGlobalFun {
    (in: Rep[Array[N]], out: Rep[Array[N]], dimZ: Rep[Int], dimY: Rep[Int], dimX: Rep[Int]) =>
      generate_comment("this is the permute kernel for [1, 0, 2]")
      generate_comment("arg0: 3D input tensor (dimZ x dimY x dimX)")
      generate_comment("arg1: 3D output tensor (dimY x dimZ x dimX)")
      generate_comment("arg2: dimZ of input")
      generate_comment("arg3: dimY of input")
      generate_comment("arg4: dimX of input")
      generate_comment("caller must use <<<dim3(dimY, dimZ, 1), dim3(A, 1, 1)>>> where A < dimX")
      generate_comment("each threadblock hands one dimX in coalease size of A, then we have dimZ x dimY threadblocks")
      generate_comment("this kernel might be inefficient if the dimX is small. TODO")

      // amazing helper function to get flatten offset of tensors
      def flatten(shape: List[Rep[Int]], index: List[Rep[Int]]): Rep[Int] = {
        val a: Rep[Int] = (shape.tail zip index.init).foldLeft(unit(0)) {
          case (z: Rep[Int], (shape: Rep[Int], index: Rep[Int])) => (index + z) * shape
          }
        a + index.last
      }

      // helper function from block indices and thread indices to input reads
      def in_offset(bX: Rep[Int], bY: Rep[Int], tX: Rep[Int]): Rep[N] = {
        in(flatten(List(dimZ, dimY, dimX), List(bY, bX, tX)))
      }
      // helper function from block indices and thread indices to output writes
      def out_offset(bX: Rep[Int], bY: Rep[Int], tX: Rep[Int], value: Rep[N]) = {
        out(flatten(List(dimY, dimZ, dimX), List(bY, bX, tX))) = value
      }
      // loop all threads in each thread box for all dimX
      // note that the block indices for input and output should be transposed
      for (i <- (0 until (dimX, blockDimX))) {
        val value = in_offset(blockIdxX, blockIdxY, threadIdxX + i)
        out_offset(blockIdxY, blockIdxX, threadIdxX + i, value)
      }
  }
}

trait CCodeGenCudaOps extends CCodeGenSizeTOps with CudaCodeGenLibFunction with CCodeGenLibs {
  // need to register the headers
  registerHeader("\"cuda_header.h\"")

  override def mayInline(n: Node): Boolean = n match {
    case Node(s, "NewSharedArray", _, _) => false
    case Node(s, "CastArray", _, _) => false 
    case _ => super.mayInline(n)
  }

  override def remap(m: Manifest[_]): String = m.runtimeClass.getName match {
    case s: String if s.endsWith("Dim3") => "dim3"
    case s: String if s.endsWith("CudaErrorT") => "cudaError_t"
    case _ => super.remap(m)
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s,"@", (f:Backend.Exp)::args, _) => graphCache(f.asInstanceOf[Sym]) match {
      case Node(_, "λ", (b: Block)::Backend.Const(arity:Int)::Backend.Const("__global__")::Nil, _) =>
        shallowP(f);
        assert(args.size > arity + 1, "size of args should be at least arity + 2")
        val dims = args.drop(arity)
        val other_args = args.take(arity)
        emit("<<<"); shallow(dims.head); dims.tail.foreach(d => {emit(", "); shallow(d)}); emit(">>>");
        emit("("); other_args.headOption.foreach(h => { shallowP(h, 0); other_args.tail.foreach(a => { emit(", "); shallowP(a, 0) }) }); emit(")")
      case _ => super.shallow(n)
    }
    case _ => super.shallow(n)
  }

  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "NewSharedArray", xs, _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit("__shared__ "); emit(s"$tpe "); shallow(s);
      xs.foreach {x => emit("["); shallow(x); emit("]") }; emitln(";")
    case n @ Node(s, "NewDynSharedArray", List(), _) =>
      val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
      emit("extern __shared__ "); emit(s"$tpe "); shallow(s); emitln("[];")
    case n @ Node(s, "CastArray", xs, _) =>
        val tpe = remap(typeMap.get(s).map(_.typeArguments.head).getOrElse(manifest[Unknown]))
        emit(s"$tpe* "); shallow(s); emit(s" = ($tpe *)("); shallow(xs(0)); emitln(");")
    case n @ Node(s, "cudaArrayEffect", arr::lhs::rhs::_, _) =>
      shallow(arr); emit("["); shallow(lhs); emit("] = "); shallow(rhs); emitln(";")
    case _ => super.traverse(n)
  }
}
