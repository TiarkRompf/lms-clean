package lms
package core

import stub._
import macros.SourceContext

class InliningTest extends TutorialFunSuite {
  val under = "backend/"

  test("inlining_1") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val res = arg - 4
        val y = if (res == 0) arg + 4 else res
        printf("%d\n", y)
      }
    }
    val src = driver.code
    checkOut("inlining_1", "c", {
      println(src)
      println("// output:")
      driver.eval(4)
      driver.eval(5)
    })
  }

  test("inlining_2") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val res = arg - 4
        var i = 0
        while (i < arg) {
          printf("%d\n", res)
          i += 1
        }
      }
    }
    val src = driver.code
    checkOut("inlining_2", "c", {
      println(src)
      println("// output:")
      driver.eval(4)
      driver.eval(5)
    })
  }
}
