package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.{SourceContext, RefinedManifest}
import lms.collection.mutable.{ArrayOps, ArrayTypeLess}

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
object ArrayCPUTypeLess extends Dsl with ArrayOps {
  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._

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

  // This is the typeless frontend for printing all elements of an ARRAY (in flat format)
  def ARRAY_PRINT(a: ARRAY, size: INT)(implicit __pos: SourceContext) = {
    // Similarly, we are using the typed front-end of the for loop
    for (i <- (0 until Wrap[Int](size.x)): Rep[Range]) {
      val index = INT(Unwrap(i))
      // here we have to decide which formatting string to use for printing,
      // and we do so by match-casing the type manifest
      a.et match {
        case n if n == manifest[Int] => printf("%d ", Wrap[Int](a(index).x))
        case n if n == manifest[Float] => printf("%f ", Wrap[Float](a(index).x))
        case n => System.out.println(s"manifest $n is not supported yet in ARRAY_PRINT")
      }
    }
  }
}


trait ArrayCPUOps extends Dsl with ArrayOps {

  // This is the typed frontend for adding 2 arrays element-wise (no broadcasting)
  // We could just do a shallow wrapping of ARRAY_ADD, but if the implementation is super simple,
  //   we can also just re-implement it.
  def array_add[T:Numeric:Manifest](a: Rep[Array[T]], b: Rep[Array[T]], res: Rep[Array[T]], size: Int)(implicit __pos: SourceContext) = {
    for (i <- (0 until size): Rep[Range]) {
      res(i) = a(i) + b(i)
    }
  }

  // This is the typed frontend for printing all elements of a Rep[Array[T]] (in flat format)
  def array_print[T:Manifest](a: Rep[Array[T]], size: Int)(implicit __pos: SourceContext) = {
    for (i <- (0 until size): Rep[Range]) {
      manifest[T] match {
        case n if n == manifest[Int] => printf("%d ", a(i))
        case n if n == manifest[Float] => printf("%f ", a(i))
        case n => System.out.println(s"manifest $n is not supported yet in ARRAY_PRINT")
      }
    }
  }
}

