package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.{SourceContext, RefinedManifest}
import lms.thirdparty.{CLibs, SizeTOps, CBLASOps}
import lms.collection.mutable.ArrayOps

trait CBLASOps extends Base with CLibs with SizeTOps with ArrayOps {

  abstract class CBLAS_LAYOUT
  def rowMajor = cmacro[CBLAS_LAYOUT]("CblasRowMajor")

  abstract class CBLAS_TRANSPOSE
  def noTrans = cmacro[CBLAS_TRANSPOSE]("CblasNoTrans")
  def yesTrans = cmacro[CBLAS_TRANSPOSE]("CblasTrans")

  /*
    void cblas_sgemv(const CBLAS_LAYOUT layout,
                const CBLAS_TRANSPOSE TransA, const int M, const int N,
                const float alpha, const float  *A, const int lda,
                const float  *X, const int incX, const float beta,
                float  *Y, const int incY)
  */
  def cblas_sgemv(layout: Rep[CBLAS_LAYOUT], transA: Rep[CBLAS_TRANSPOSE], m: Rep[Int], n: Rep[Int],
      alpha: Rep[Float], A: Rep[Array[Float]], lda: Rep[Int], X: Rep[Array[Float]], incX: Rep[Int],
      beta: Rep[Float], Y: Rep[Array[Float]], incY: Rep[Int]) =
    libFunction[Unit]("cblas_sgemv", Unwrap(layout), Unwrap(transA), Unwrap(m), Unwrap(n), Unwrap(alpha),
      Unwrap(A), Unwrap(lda), Unwrap(X), Unwrap(incX), Unwrap(beta), Unwrap(Y),
      Unwrap(incY))(Seq(0, 1, 5, 7), Seq(10), Set[Int]())

  def CBLAS_SGEMV(layout: Rep[CBLAS_LAYOUT], transA: Rep[CBLAS_TRANSPOSE], m: INT, n: INT,
      alpha: FLOAT, a: ARRAY, lda: INT, x: ARRAY, incX: INT, beta: FLOAT, y: ARRAY, incY: INT) = {
    // this function only works for float arrays I think
    assert(a.et == manifest[Float] && x.et == manifest[Float] && y.et == manifest[Float])
    libFunction[Unit]("cblas_sgemv", Unwrap(layout), Unwrap(transA), m.x, n.x, alpha.x,
      a.x, lda.x, x.x, incX.x, beta.x, y.x, incY.x)(Seq(0, 1, 5, 7), Seq(10), Set[Int]())
  }

  /*
    void cblas_sgemm(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
                const CBLAS_TRANSPOSE TransB, const int M, const int N,
                const int K, const float alpha, const float  *A,
                const int lda, const float  *B, const int ldb,
                const float beta, float  *C, const int ldc)
  */
  def cblas_sgemm(layout: Rep[CBLAS_LAYOUT], transA: Rep[CBLAS_TRANSPOSE], transB: Rep[CBLAS_TRANSPOSE],
      m: Rep[Int], n: Rep[Int], k: Rep[Int], alpha: Rep[Float], A: Rep[Array[Float]], lda: Rep[Int],
      B: Rep[Array[Float]], ldb: Rep[Int], beta: Rep[Float], C: Rep[Array[Float]], ldc: Rep[Int]) =
    libFunction[Unit]("cblas_sgemm", Unwrap(layout), Unwrap(transA), Unwrap(transB), Unwrap(m), Unwrap(n),
      Unwrap(k), Unwrap(alpha), Unwrap(A), Unwrap(lda), Unwrap(B), Unwrap(ldb), Unwrap(beta), Unwrap(C),
      Unwrap(ldc))(Seq(0, 1, 2, 7, 9), Seq(12), Set[Int]())

  def CBLAS_SGEMM(layout: Rep[CBLAS_LAYOUT], transA: Rep[CBLAS_TRANSPOSE], transB: Rep[CBLAS_TRANSPOSE],
      m: INT, n: INT, k: INT, alpha: FLOAT, a: ARRAY, lda: INT, b: ARRAY, ldb: INT, beta: FLOAT, c: ARRAY,
      ldc: INT) = {
    // this function only works for float arrays I think
    assert(a.et == manifest[Float] && b.et == manifest[Float] && c.et == manifest[Float])
    libFunction[Unit]("cblas_sgemm", Unwrap(layout), Unwrap(transA), Unwrap(transB), m.x, n.x, k.x,
      alpha.x, a.x, lda.x, b.x, ldb.x, beta.x, c.x, ldc.x)(Seq(0, 1, 2, 7, 9), Seq(12), Set[Int]())
  }

}

trait CCodeGenCBLASOps extends CCodeGenLibs {
  registerHeader("/opt/OpenBLAS/include", "<cblas.h>")
  registerLibrary("-lopenblas")
}
