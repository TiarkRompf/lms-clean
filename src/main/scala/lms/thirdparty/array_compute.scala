package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.{SourceContext, RefinedManifest}
import lms.collection.mutable.ArrayOps

/**
 * This frontend is used for Tensor Computations by CPU (naive implementation)
 * However, it is implemented at array level because many tensor computation libraries directly
 *     works with arrays.
 *
 * We are also trying to present two styles of frontends: the typeless (all caps letters) and the typed
 *     The typeless frontend is easier to use in IR transformation.
 *     The typed frontend is more intuitive to use directly by the DSL user.
 * Likely the typed frontend will just be a shallow wrapper of the typeless frontend, so
 *     that we have no code duplication.
 */
trait ArrayCPUOps extends Dsl with ArrayOps {

  // This is the typeless frontend for adding 2 arrays element-wise (no broadcasting)
  def ARRAY_ADD(a: ARRAY, b: ARRAY, res: ARRAY, size: INT)(implicit _pos: SourceContext) = {
    // it might be unclean to use typed frontend of for loop in a typeless frontend function
    // however, in this function, we know for sure that the iteration is over Int-typed indices
    // and the result type of for loop body is Unit (everything is side-effect)
    // So we should be able to use typed frontend of for loop just fine :)
    // It is admittedly not very clean.
    for (i <- (0 until Wrap[Int](size.x)): Rep[Range]) {
      val index = INT(Unwrap(i)) // need this to convert Rep[Int] to INT :(
      res(index) = a(index) + b(index)
      () // need this to enforce Rep[Unit] return type :(
    }
  }

  def array_add[T:Numeric:Manifest](a: Rep[Array[T]], b: Rep[Array[T]], size: Int)(implicit __pos: SourceContext) = {
    val res = NewArray[T](size)
    for (i <- (0 until size): Rep[Range]) {
      res(i) = a(i) + b(i)
    }
    res
  }
  def array_print[T:Manifest](a: Rep[Array[T]], size: Int)(implicit __pos: SourceContext) = {
    for (i <- (0 until size): Rep[Range]) {
      printf("%d ", a(i))
    }
  }
}

