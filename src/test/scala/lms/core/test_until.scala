package lms
package core

import stub._
import macros.SourceContext

class UntilTest extends TutorialFunSuite {
  val under = "backend/"

  test("until") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        for (i <- arg until arg * 2)
          printf("%d\n", i)
      }
    }
    check("until", driver.code, "c")
  }

  test("until_step") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        for (i <- arg.until(arg * 2, unit(2)))
          printf("%d\n", i)
      }
    }
    check("until_step", driver.code, "c")
  }
}
