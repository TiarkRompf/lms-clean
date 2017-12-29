/**
Rewrites with Generic Types
===========================

*/

package scala.lms

import scala.annotation.implicitNotFound

class RewriteTest extends TutorialFunSuite {
  val under = "rewrite-"

  val IR: DSL with BaseExp = new Impl {}
  import IR._ // Int means IR.Int
  import codeBuilder.Exp

  def rewrite[A:Typ,B:Typ,C:Typ](f: (A,B) => Rewrite[C]): Unit = {
    val a = typ[A].from(Exp("?A"))
    val b = typ[B].from(Exp("?B"))
    val rw = codeBuilder.reifyPattern(f(a,b))
    val u = typ[C].to(rw.a)
    val v = typ[C].to(rw.b)
    println("rewrite: " + u + " ===> " + v)
  }


  test("01") {
    val res = utils.captureOut {

      def foo1(x: Int, y: Int): Int = reflect[Int]("foo1",ref(x),ref(y))
      def foo2(x: Int, y: Int): Int = reflect[Int]("foo2",ref(x),ref(y))

      rewrite((a:Int,b:Int) => Rewrite(foo1(a,b),foo2(a,b)))

    }
    check("01", res)
  }

  test("02") {
    val res = utils.captureOut {

      def foo1[A:Typ,B:Typ](x: A, y: B): A = reflect[A]("foo1",typ[A],typ[B],ref(x),ref(y))
      def foo2[A:Typ,B:Typ](x: A, y: B): A = reflect[A]("foo2",typ[A],typ[B],ref(x),ref(y))

      case class Param1(exp:Exp)
      case class Param2(exp:Exp)

      implicit object p1typ extends Typ[Param1] {
        def from(e: RewriteTest.this.IR.codeBuilder.Exp): Param1 = Param1(e)
        def to(x: Param1): RewriteTest.this.IR.codeBuilder.Exp = x.exp
        override def toString = "P1?"
      }
      implicit object p2typ extends Typ[Param2] {
        def from(e: RewriteTest.this.IR.codeBuilder.Exp): Param2 = Param2(e)
        def to(x: Param2): RewriteTest.this.IR.codeBuilder.Exp = x.exp
        override def toString = "P2?"
      }

      object Generic {
        type T = Param1
        type U = Param2
        type T1 = Param1
        type T2 = Param2
      }


      rewrite((a: Generic.T1, b: Generic.T2) => Rewrite(foo1(a,b),foo2(a,b)))

    }
    check("02", res)
  }


}