/**
Auto-Generating the IR
======================

*/

package lms
package experimental

import lms.core.utils
import scala.annotation.implicitNotFound

class AutoTest extends TutorialFunSuite {
  val under = "experimental/auto-"

  test("one") {
    val res = utils.captureOut {
      val IR: DSL with BaseExp = new Impl {}
      import IR._ // Int means IR.Int

      @ir def test1(x: Int, y: => Int): Int
      // transformed to:
      // def test1(x: Int, y: => Int): Int = reflect[Int]("test1",ref(x),ref(y))

      @ir def test2(x: Int, y: Int): Int = 666
      // transformed to:
      // def test2(x: Int, y: Int): Int = reflect[Int]("test2",ref(x),ref(y))
      // def test2_next(x: Int, y: Int): Int = 666
      // lower((x: Int, y: Int) => Rewrite(test2(x,y),test2_next(x,y))

      println(test1(3,7))

      println(test2(3,7))

      println(test2_next(3,7))

    }
    check("one", res)
  }

}