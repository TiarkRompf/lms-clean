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
import lms.thirdparty.{CLibs, CCodeGenLibs, CLibTypeLess, SizeTOps, SIZE_TTypeLess, CCodeGenSizeTOps}


object CUDATypeLess extends Dsl with StackArrayOps with CLibs with CudaFunction {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import FixedSizeTensorDeviceTypeLess._
  import CLibTypeLess._
  import SIZE_TTypeLess._

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


  class DIM3(override val x: Backend.Exp) extends TOP(x)
  def DIM3(a: Int, b: Int = 1, c: Int = 1)(implicit __pos: SourceContext): DIM3 =
    (new DIM3(Unwrap(libFunction("dim3", Backend.Const(a), Backend.Const(b), Backend.Const(c))(Seq[Int](),
      Seq[Int](), Set[Int](), Backend.UNSAFE)))).withSource(__pos)

  def CUDA_KERNEL3(f: List[Backend.Exp] => Backend.Exp, ms: Manifest[_]*)(implicit __pos: SourceContext) = {
    val kernel = Adapter.g.reflect("λ", Adapter.g.reify(3, f), Backend.Const(0), Backend.Const("__global__"))
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
    val kernel = Adapter.g.reflect("λ", Adapter.g.reify(4, f), Backend.Const(0), Backend.Const("__global__"))
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
    val kernel = Adapter.g.reflect("λ", Adapter.g.reify(5, f), Backend.Const(0), Backend.Const("__global__"))
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
  def blockDimX(implicit __pos: SourceContext): INT = INT(CMACRO("blockDim.x", manifest[Int]))
  def blockDimY(implicit __pos: SourceContext): INT = INT(CMACRO("blockDim.y", manifest[Int]))
  def blockDimZ(implicit __pos: SourceContext): INT = INT(CMACRO("blockDim.z", manifest[Int]))

  def blockIdxX(implicit __pos: SourceContext): INT = INT(CMACRO("blockIdx.x", manifest[Int]))
  def blockIdxY(implicit __pos: SourceContext): INT = INT(CMACRO("blockIdx.y", manifest[Int]))
  def threadIdxX(implicit __pos: SourceContext): INT = INT(CMACRO("threadIdx.x", manifest[Int]))
  def threadIdxY(implicit __pos: SourceContext): INT = INT(CMACRO("threadIdx.y", manifest[Int]))
  def threadIdxZ(implicit __pos: SourceContext): INT = INT(CMACRO("threadIdx.z", manifest[Int]))

  def blockRows(implicit __pos: SourceContext): INT = INT(CMACRO("BLOCK_ROWS", manifest[INT]))

  val gridSize = 28
  val blockSize = 512
  val tileDim = 32

  def CUDA_FILL_KERNEL(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_KERNEL3({xn: List[Backend.Exp] =>
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
      for (i <- range_until_step(Wrap[Int](tid.x), Wrap[Int]((dim0*dim1).x), Wrap[Int](stride.x))) {
        val index = INT(Unwrap(i))
        val size0 = index / dim0
        val size1 = index % dim0
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
  def gridDimX: Rep[Int] = cmacro[Int]("gridDim.x")
  def gridDimY: Rep[Int] = cmacro[Int]("gridDim.y")
  def blockDimX: Rep[Int] = cmacro[Int]("blockDim.x")
  def blockDimY: Rep[Int] = cmacro[Int]("blockDim.y")
  def blockDimZ: Rep[Int] = cmacro[Int]("blockDim.z")

  def blockIdxX: Rep[Int] = cmacro[Int]("blockIdx.x")
  def blockIdxY: Rep[Int] = cmacro[Int]("blockIdx.y")
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
}

trait CCodeGenCudaOps extends CCodeGenSizeTOps with CudaCodeGenLibFunction with CCodeGenLibs {
  // need to register the headers
  registerHeader("\"cuda_header.h\"")

  override def remap(m: Manifest[_]): String = m.runtimeClass.getName match {
    case s: String if s.endsWith("Dim3") => "dim3"
    case s: String if s.endsWith("CudaErrorT") => "cudaError_t"
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
