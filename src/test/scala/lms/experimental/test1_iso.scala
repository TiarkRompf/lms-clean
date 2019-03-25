/**
LMS without Rep?
================

In this file, we explore a slightly different front-end 
embedding for LMS.

Where we normally have a type distinction between, say:
  
  Int and Rep[Int] (now vs later)

We now use:

  scala.Int vs IR.Int

The question now is, of course, what do we do for things like 
if/then/else, which take a staged expression Rep[T], for any T?

The solution is simple: if/then/else just takes a T, but we use a 
type class T:Typ to denote that T is a DSL type. This type 
class acts as an isomorphism that converts from the user-visible 
type to the interal IR and back.

This model is heavily inspired by Feldspar, Scalan, and also by
the emerging use of type classes in Spiral (ExposeRep) and
Delite (Repable).

The main benefit is that the DSL interface can get away 
without *any* implicit conversions, and should therefore 
drastically cut down on compile-times.
*/

package lms
package experimental

import lms.core.virtualize
import lms.core.utils

class IsoTest extends TutorialFunSuite {
  val under = "experimental/iso-"

/**
Test Case (Client Code)
-----------------------

There are no Rep types in client code, and we can still
leverage staging since we have acces to both scala.Int 
and IR.Int.
*/
  test("one") {
    val IR: DSL = new Impl {}
    @virtualize def code = {
      import IR._

      // primitives: Int resolves to IR.Int as opposed to scala.Int (due to import)

      val x: Int = 5

      // conditional: 3 and 7 (type scala.Int) are automatically lifted
      // (would need expected type Int for non-virtualized if)

      val y = if (true) 3 + x else 7

      // arrays: constructor requires T:Typ

      val xs = NewArray[Int](7)

      xs(y) = x

      // nested arrays work as well

      val ys = NewArray[Array[Int]](1)

      ys(0) = xs      
    }
    val res = utils.captureOut(code)
    check("one", res)
  }

/**
Generated Code
--------------

See src/out/iso_one.check.scala:

    val x1 = 3+5
    val x2 = if (true) x1 else 7
    val x3 = new Array[Int](7)
    val x4 = x3(x2) = 5
    val x5 = new Array[Array[Int]](1)
    val x6 = x5(0) = x3


Discussion
----------

In choosing to import IR._ above, we made a choice that the DSL 
takes precedence over the meta language. Another option is to import
things selectively, such that Int refers to scala.Int, and one
has to use IR.Int to get the DSL version.

Some differences to consider compared to Rep:

  (1)  Rep[Array[(Int,Float)]]  -->  IR.Array[IR.Pair[IR.Int,IR.Float]]
  (2)  MaybeRep[T]              -->  (need a separate impl of type T)


Internals
---------

The internal implementation is fairly standard LMS IR.
It is stripped down here to be self-contained.
*/


}