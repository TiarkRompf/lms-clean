/**
Auto-Generating the IR
======================

*/

package scala.lms

import scala.annotation.implicitNotFound

class AutoTest extends TutorialFunSuite {
  val under = "auto-"

  test("one") {
    val res = utils.captureOut {
      val IR: Impl = new Impl {}
      import IR._

      case class MyInt(s:String)

      implicit def intTyp = new Typ[MyInt] {
        def to(x:MyInt): Exp = Exp(x.s)
        def from(x:Exp): MyInt = MyInt(x.s)
      }

      implicit def intX(x:scala.Int) = MyInt(x.toString)
      

      @ir def test1(x: MyInt, y: => MyInt): MyInt

      @ir def test2(x: MyInt, y: MyInt): MyInt = 666


      println(test1(3,7))

      println(test2(3,7))

      println(test2_next(3,7))

    }
    check("one", res)
  }

}