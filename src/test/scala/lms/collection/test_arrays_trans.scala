package lms
package core

import stub._
import macros.SourceContext

class ArrayTransTest extends TutorialFunSuite {
  val under = "backend/"

  test("free-1") {
    val driver = new CompilerC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        printf("%d\n", x(0))
        x.free
      }
    }
    check("free_1", driver.code, "c")
  }
}
