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
      val IR: DSL with BaseExp = new Impl {}
      import IR._ // Int means IR.Int

      @ir def test1(x: Int, y: => Int): Int

      @ir def test2(x: Int, y: Int): Int = 666


      println(test1(3,7))

      println(test2(3,7))

      println(test2_next(3,7))

    }
    check("one", res)
  }

}