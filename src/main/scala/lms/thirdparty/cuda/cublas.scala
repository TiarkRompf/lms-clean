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


object CUBLASTypeLess extends Dsl with CLibs {

    import BaseTypeLess._
    import PrimitiveTypeLess._
    import ArrayTypeLess._
    import CLibTypeLess._

    def CUBLAS_CALL(status: Backend.Exp)(implicit __pos: SourceContext) =
    UNIT(Unwrap(libFunction[Unit]("CUBLAS_CALL", status)(Seq[Int](0), Seq[Int](), Set[Int](), Adapter.CTRL)))

    class CUBLAS_HANDLE(override val x: Backend.Exp) extends TOP(x)

    var cache: Option[CUBLAS_HANDLE] = None
    def CUBLAS_HANDLE(implicit __pos: SourceContext) = cache match {
      case Some(x) => x
      case None =>
        val handle = new CUBLAS_HANDLE(NEW_STRUCT(manifest[CUBLAS_HANDLE], "cublasHandle_t").x)
        CUBLAS_CREATE(handle)
        cache = Some(handle)
        handle
    }

    def set_up_cublas(implicit __pos: SourceContext) = { val dummy = CUBLAS_HANDLE }
    def finalize_cublas(implicit __pos: SourceContext) = { CUBLAS_DESTROY(CUBLAS_HANDLE) }

    def CUBLAS_CREATE(handle: CUBLAS_HANDLE)(implicit __pos: SourceContext) =
    CUBLAS_CALL(Unwrap(libFunction[Any]("cublasCreate", handle.x)(Seq[Int](), Seq(0), Set(0))))
    def CUBLAS_DESTROY(handle: CUBLAS_HANDLE)(implicit __pos: SourceContext) =
    CUBLAS_CALL(Unwrap(libFunction[Any]("cublasDestroy", handle.x)(Seq[Int](), Seq(0), Set[Int]())))

    class CUBLAS_OPERATION(override val x: Backend.Exp) extends TOP(x)
    def CUBLAS_OPERATION(x: TOP) = new CUBLAS_OPERATION(x.x)
    def CUBLAS_OP_N(implicit __pos: SourceContext) = CUBLAS_OPERATION(CMACRO("CUBLAS_OP_N", manifest[CUBLAS_OPERATION]))
    def CUBLAS_OP_T(implicit __pos: SourceContext) = CUBLAS_OPERATION(CMACRO("CUBLAS_OP_T", manifest[CUBLAS_OPERATION]))

    def ZERO(implicit __pos: SourceContext) = FLOAT(0.0f)
    def ONE(implicit __pos: SourceContext) = FLOAT(1.0f)
    def MINUS_ONE(implicit __pos: SourceContext) = FLOAT(-1.0f)

    /*
    cublasStatus_t cublasSdot (cublasHandle_t handle, int n,
                               const float           *x, int incx,
                               const float           *y, int incy,
                               float           *result)
    This library function computes the dot product of vectors x and y.
    incx: stride between consecutive elements of x
    incy: stride between consecutive elements of y
    */
    def CUBLAS_SDOT(handle: CUBLAS_HANDLE, n: INT, x: ARRAY, incx: INT, y: ARRAY, incy: INT, result: ARRAY)(implicit __pos: SourceContext) = {
      assert(x.et == manifest[Float] && y.et == manifest[Float] && result.et == manifest[Float])
      CUBLAS_CALL(Unwrap(libFunction[Any]("cublasSdot", handle.x, n.x, x.x, incx.x, y.x, incy.x,
        result.x)(Seq(0, 2, 4), Seq(6), Set[Int]())))
    }

    /*
    cublasStatus_t cublasSgemv(cublasHandle_t handle, cublasOperation_t trans,
                            int m, int n,
                            const float           *alpha,
                            const float           *A, int lda,
                            const float           *x, int incx,
                            const float           *beta,
                            float           *y, int incy)
    This library function performs the matrix-vector multiplication
    y = alpha * (op(A) mat-vec-mul x) + beta * y
    where A is a m x n matrix stored in column-major format, x and y are vectors,
    alpha and beta are scalars.
    op(A) = A   if transa == CUBLAS_OP_N
            A^T if transa == CUBLAS_OP_T
            A^H if transa == CUBLAS_OP_H
    lda: leading dimension of two-dimensional array used to store matrix A
    */
    def CUBLAS_SGEMV(handle: CUBLAS_HANDLE, trans: CUBLAS_OPERATION, m: INT, n: INT, alpha: FLOAT,
                    A: ARRAY, lda: INT, x: ARRAY, incx: INT, beta: FLOAT, y: ARRAY, incy: INT)(implicit __pos: SourceContext) = {
      assert(A.et == manifest[Float] && x.et == manifest[Float] && y.et == manifest[Float])
      CUBLAS_CALL(Unwrap(libFunction[Any]("cublasSgemv", handle.x, trans.x, m.x, n.x, alpha.x, A.x, lda.x, x.x, incx.x,
        beta.x, y.x, incy.x)(Seq(0, 5, 7, 10), Seq(10), Set(4, 9))))
    }

    /*
    cublasStatus_t cublasSgeam(cublasHandle_t handle,
                            cublasOperation_t transa, cublasOperation_t transb,
                            int m, int n,
                            const float           *alpha,
                            const float           *A, int lda,
                            const float           *beta,
                            const float           *B, int ldb,
                            float           *C, int ldc)
    This library function performs the matrix-matrix addition/transposition
    C = alpha * op(A) + beta * op(B)
    where alpha and beta are scalars, and A, B, and C are matrices stored in column-major format with
    dimention (m x n)
    op(A) = A   if transa == CUBLAS_OP_N
            A^T if transa == CUBLAS_OP_T
            A^H if transa == CUBLAS_OP_C
    For in-place mode, if C = A, then ldc = lda and transa == CUBLAS_OP_N
                       if C = B, then ldc = ldb and transb == CUBLAS_OP_N
    lda: leading dimension of two-dimensional array used to store matrix A
    ldb: leading dimension of two-dimensional array used to store matrix B
    ldc: leading dimension of two-dimensional array used to store matrix C
    */
    def CUBLAS_SGEAM(handle: CUBLAS_HANDLE, transa: CUBLAS_OPERATION, transb: CUBLAS_OPERATION,
                    m: INT, n: INT, alpha: FLOAT, A: ARRAY, lda: INT, beta: FLOAT, B: ARRAY, ldb: INT, C: ARRAY, ldc: INT)(implicit __pos: SourceContext) = {
      assert(A.et == manifest[Float] && B.et == manifest[Float] && C.et == manifest[Float])
      CUBLAS_CALL(Unwrap(libFunction[Any]("cublasSgeam", handle.x, transa.x, transb.x, m.x, n.x, alpha.x,
        A.x, lda.x, beta.x, B.x, ldb.x, C.x, ldc.x)(Seq(0, 6, 9, 11), Seq(11), Set(5, 8))))
    }

    /*
    From https://docs.nvidia.com/cuda/cublas/index.html
    cublasStatus_t cublasSgemm(cublasHandle_t handle,
                            cublasOperation_t transa, cublasOperation_t transb,
                            int m, int n, int k,
                            const float           *alpha,
                            const float           *A, int lda,
                            const float           *B, int ldb,
                            const float           *beta,
                            float           *C, int ldc)
    This library function performs the matrix-matrix multiplication
    C = alpha * (op(A) matmul op(B)) + beta * C
    where alpha and beta are scalars, and A, B, C are matrices stored in column-major format
    with dimensions op(A): m x k, op(B): k x n, and C: m x n, respectively.
    op(A) = A   if transa == CUBLAS_OP_N
            A^T if transa == CUBALS_OP_T
            A^H if transa == CUBLAS_OP_C
    lda: leading dimension of two-dimensional array used to store matrix A
    ldb: leading dimension of two-dimensional array used to store matrix B
    ldc: leading dimension of two-dimensional array used to store matrix C
    */
    def CUBLAS_SGEMM(handle: CUBLAS_HANDLE, transa: CUBLAS_OPERATION, transb: CUBLAS_OPERATION,
                    m: INT, n: INT, k: INT, alpha: FLOAT, A: ARRAY, lda: INT, B: ARRAY, ldb: INT, beta: FLOAT, C: ARRAY, ldc: INT)(implicit __pos: SourceContext) = {
      // FIXME(feiw) have a bug such that A.et is empty
      // assert(A.et == manifest[Float] && B.et == manifest[Float] && C.et == manifest[Float])
      CUBLAS_CALL(Unwrap(libFunction[Any]("cublasSgemm", handle.x, transa.x, transb.x, m.x, n.x, k.x, alpha.x,
        A.x, lda.x, B.x, ldb.x, beta.x, C.x, ldc.x)(Seq(0,7,9,12), Seq(12), Set(6, 11))))
    }
}

trait CuBLASOps extends Base with CLibs with CudaFunction with StackArrayOps with SizeTOps {

  abstract class CublasStatusT
  def cublasCall(status: Rep[CublasStatusT]) =
    libFunction[Unit]("CUBLAS_CALL", Unwrap(status))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  abstract class CublasHandleT
  lazy val cublasHandle = newStruct[CublasHandleT]("cublasHandle_t")
  def cublasCreate(handle: Rep[CublasHandleT]) =
    libFunction[CublasStatusT]("cublasCreate", Unwrap(handle))(Seq[Int](), Seq(0), Set(0))
  def cublasDestroy(handle: Rep[CublasHandleT]) =
    libFunction[CublasStatusT]("cublasDestroy", Unwrap(handle))(Seq[Int](), Seq(0), Set[Int]())

  abstract class CublasOperationT
  def cublasOpN = cmacro[CublasOperationT]("CUBLAS_OP_N")
  def cublasOpT = cmacro[CublasOperationT]("CUBLAS_OP_T")
  def INFINITY = cmacro[Float]("INFINITY")

  lazy val zero = var_new(0.0f)
  lazy val one = var_new(1.0f)
  lazy val minus_one = var_new(-1.0f)

  /*
    cublasStatus_t cublasSdot (cublasHandle_t handle, int n,
                           const float           *x, int incx,
                           const float           *y, int incy,
                           float           *result)
   */
  def cublasSdot(handle: Rep[CublasHandleT], n: Rep[Int], x: Rep[Array[Float]], incx: Rep[Int],
                  y: Rep[Array[Float]], incy: Rep[Int], result: Rep[Array[Float]]) =
    libFunction[CublasStatusT]("cublasSdot",
      Unwrap(handle), Unwrap(n), Unwrap(x), Unwrap(incx), Unwrap(y), Unwrap(incy), Unwrap(result))(Seq(0, 2, 4), Seq(6), Set[Int]())

  /*
    matching syntax of this library function
    cublasStatus_t cublasSgemv(cublasHandle_t handle, cublasOperation_t trans,
                           int m, int n,
                           const float           *alpha,
                           const float           *A, int lda,
                           const float           *x, int incx,
                           const float           *beta,
                           float           *y, int incy)
  */
  def cublasSgemv(handle: Rep[CublasHandleT], trans: Rep[CublasOperationT], m: Rep[Int], n: Rep[Int], alpha: Var[Float],
                   A: Rep[Array[Float]], lda: Rep[Int], x: Rep[Array[Float]], incx: Rep[Int], beta: Var[Float],
                   y: Rep[Array[Float]], incy: Rep[Int]): Rep[CublasStatusT] =
    libFunction[CublasStatusT]("cublasSgemv",
      Unwrap(handle), Unwrap(trans), Unwrap(m), Unwrap(n), UnwrapV(alpha), Unwrap(A), Unwrap(lda), Unwrap(x), Unwrap(incx),
      UnwrapV(beta), Unwrap(y), Unwrap(incy))(Seq(0,5,7,10), Seq(10), Set(4, 9))

  /*
    matching syntax of this library function
    cublasStatus_t cublasSgeam(cublasHandle_t handle,
                          cublasOperation_t transa, cublasOperation_t transb,
                          int m, int n,
                          const float           *alpha,
                          const float           *A, int lda,
                          const float           *beta,
                          const float           *B, int ldb,
                          float           *C, int ldc)
  */
  def cublasSgeam(handle: Rep[CublasHandleT], transa: Rep[CublasOperationT], transb: Rep[CublasOperationT],
                    m: Rep[Int], n: Rep[Int], alpha: Var[Float], A: Rep[Array[Float]], lda: Rep[Int],
                    beta: Var[Float], B: Rep[Array[Float]], ldb: Rep[Int],
                    C: Rep[Array[Float]], ldc: Rep[Int]): Rep[CublasStatusT] =
    libFunction[CublasStatusT]("cublasSgeam",
      Unwrap(handle), Unwrap(transa), Unwrap(transb), Unwrap(m), Unwrap(n), UnwrapV(alpha),
      Unwrap(A), Unwrap(lda), UnwrapV(beta), Unwrap(B), Unwrap(ldb), Unwrap(C), Unwrap(ldc))(Seq(0, 6, 9, 11), Seq(11), Set(5, 8))

  /*
    cublasStatus_t cublasSgemm(cublasHandle_t handle,
                           cublasOperation_t transa, cublasOperation_t transb,
                           int m, int n, int k,
                           const float           *alpha,
                           const float           *A, int lda,
                           const float           *B, int ldb,
                           const float           *beta,
                           float           *C, int ldc)
   */
  def cublasSgemm(handle: Rep[CublasHandleT], transa: Rep[CublasOperationT], transb: Rep[CublasOperationT],
                   m: Rep[Int], n: Rep[Int], k: Rep[Int], alpha: Var[Float], A: Rep[Array[Float]], lda: Rep[Int],
                   B: Rep[Array[Float]], ldb: Rep[Int], beta: Var[Float], C: Rep[Array[Float]], ldc: Rep[Int]) =
    libFunction[CublasStatusT]("cublasSgemm",
      Unwrap(handle), Unwrap(transa), Unwrap(transb), Unwrap(m), Unwrap(n), Unwrap(k), UnwrapV(alpha),
      Unwrap(A), Unwrap(lda), Unwrap(B), Unwrap(ldb), UnwrapV(beta), Unwrap(C), Unwrap(ldc))(Seq(0,7,9,12), Seq(12), Set(6, 11))
}


trait CCodeGenCuBLASOps extends CCodeGenSizeTOps with CudaCodeGenLibFunction with CCodeGenLibs {
  registerHeader("\"cublas_header.h\"")
}
