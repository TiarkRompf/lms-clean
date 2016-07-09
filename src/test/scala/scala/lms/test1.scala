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

package scala.lms

import scala.annotation.implicitNotFound

class IsoTest extends TutorialFunSuite {
  val under = "iso-"

  trait DSL extends Base {
    trait IntOps {
      def +(y: Int): Int
      def -(y: Int): Int
      def *(y: Int): Int
      def /(y: Int): Int
      def %(y: Int): Int
    }
    trait BooleanOps {
      def &&(y: Boolean): Boolean
      def ||(y: Boolean): Boolean
      def unary_! : Boolean
    }
    type Int <: IntOps
    type Boolean <: BooleanOps
    implicit def intTyp: Typ[Int]
    implicit def intLift: Lift[scala.Int,Int]
    implicit def booleanTyp: Typ[Boolean]
    implicit def booleanLift: Lift[scala.Boolean,Boolean]

    trait ArrayOps[T] {
      def length: Int
      def apply(x: Int): T
      def update(x: Int, y: T): Unit
    }
    type Array[T] <: ArrayOps[T]
    def NewArray[T:Typ](x: Int): Array[T]
    implicit def arrayTyp[T:Typ]: Typ[Array[T]]

    def __ifThenElse[C,A,B](c:Boolean, a:A, b:B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Typ[C]): C

  }

/**
Test Case (Client Code)
-----------------------

There are no Rep types in client code, and we can still
leverage staging since we have acces to both scala.Int 
and IR.Int.
*/
  test("one") {
    val IR: DSL = new Impl {}
    val res = utils.captureOut {
      import IR._

      // primitives: Int resolves to IR.Int as opposed to scala.Int (due to import)

      val x: Int = 5

      // conditional: 3 and 7 (type scala.Int) are automatically lifted

      val y = if (true) 3 + x else 7

      // arrays: constructor requires T:Typ

      val xs = NewArray[Int](7)

      xs(y) = x

      // nested arrays work as well

      val ys = NewArray[Array[Int]](1)

      ys(0) = xs      
    }
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

  trait Base {
    // preliminaries
    @implicitNotFound("${T} is not a DSL type")
    type Typ[T]
    @implicitNotFound("${A} cannot be implicitly lifted to ${B}")
    type Lift[A,B]
    implicit def identLift[T:Typ]: Lift[T,T]
    implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U
  }

  trait BaseExp extends Base {
    var numVars = 0
    case class Exp(s: String) { override def toString = s }
    implicit def reflect(s: String) = { numVars += 1; println("val x"+numVars+" = "+s); Exp("x"+numVars) }

    trait Typ[T] {
      def from(e:Exp): T
      def to(x:T):Exp
    }
    trait Lift[A,B] {
      def to(x:A):B
    }
    def identLift[T:Typ]: Lift[T,T] = new Lift[T,T] { def to(x:T) = x }
    def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.to(x)

    def typ[T:Typ] = implicitly[Typ[T]]
  }

  trait Impl extends BaseExp with DSL {
    case class Int(e: Exp) extends IntOps {
      def +(y: Int) = Int(e+"+"+y.e)
      def -(y: Int) = Int(e+"-"+y.e)
      def *(y: Int) = Int(e+"*"+y.e)
      def /(y: Int) = Int(e+"/"+y.e)
      def %(y: Int) = Int(e+"%"+y.e)
    }
    case class Boolean(e: Exp) extends BooleanOps {
      def &&(y: Boolean) = Boolean(e+"+"+y.e)
      def ||(y: Boolean) = Boolean(e+"+"+y.e)
      def unary_! = Boolean("!"+e)
    }

    val intTyp: Typ[Int] = new Typ[Int] { def from(e:Exp) = Int(e); def to(x:Int) = x.e; override def toString = "Int" }
    val booleanTyp: Typ[Boolean] = new Typ[Boolean] { def from(e:Exp) = Boolean(e); def to(x:Boolean) = x.e; override def toString = "Boolean" }

    val intLift: Lift[scala.Int,Int] = new Lift[scala.Int,Int] { def to(x:scala.Int) = Int(Exp(x.toString)) }
    val booleanLift: Lift[scala.Boolean,Boolean] = new Lift[scala.Boolean,Boolean] { def to(x:scala.Boolean) = Boolean(Exp(x.toString)) }
    
    case class Array[T:Typ](e: Exp) extends ArrayOps[T] {
      def length = Int(e+".length")
      def apply(x: Int) = typ[T].from(e+"("+x.e+")")
      def update(x: Int, y: T): Unit = reflect(e+"("+x.e+") = "+typ[T].to(y))
    }
    def NewArray[T:Typ](x: Int): Array[T] = Array("new Array["+typ[T]+"]("+x.e+")")
    implicit def arrayTyp[T:Typ]: Typ[Array[T]] = new Typ[Array[T]] { def from(e:Exp) = Array(e); def to(x:Array[T]) = x.e; override def toString = "Array["+typ[T]+"]" }

    def __ifThenElse[C,A,B](c:Boolean, a:A, b:B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Typ[C]): C = {
      mC.from("if ("+c.e+") "+mC.to(mA.to(a))+" else "+mC.to(mB.to(b)))
    }
  }


}