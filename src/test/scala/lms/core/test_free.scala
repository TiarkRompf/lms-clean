package lms
package core

import stub._
import macros.SourceContext

class FreeTest extends TutorialFunSuite {
  val under = "backend/"

  test("free-1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        printf("%d\n", x(0))
        x.free
      }
    }
    check("free_1", driver.code, "c")
  }

  test("free-2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        var idx = 0
        while (idx < 10) {
          x(idx) = idx
          idx += 1
        }
        printf(">> %d\n", x(5))
        x.free
      }
    }
    check("free_2", driver.code, "c")
  }

  test("free-dead1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        x.free
      }
    }
    check("free_dead1", driver.code, "c")
  }

  test("free-dead2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        x(0) = 1
        x.free
      }
    }
    check("free_dead2", driver.code, "c")
  }

  test("free-dead3") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        val y = NewArray[Int](10)
        printf("%d\n", x(0))
        y.free
        x.free
      }
    }
    check("free_dead3", driver.code, "c")
  }

  test("free-dead4") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        var idx = 0
        while (idx < 10) {
          x(idx) = idx
          idx += 1
        }
        x.free
      }
    }
    check("free_dead4", driver.code, "c")
  }
}
