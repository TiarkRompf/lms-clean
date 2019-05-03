package lms
package core

import stub._
import macros.SourceContext

class VariablesTest extends TutorialFunSuite {
  val under = "backend/"

  test("variable_r") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val y: Rep[Int] = x
        val z: Rep[Int] = x
        printf("%d\n", y + z)
      }
    }
    check("variable_r", driver.code, "c")
  }

  test("variable_w_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        x = 3
        printf("A\n")
      }
    }
    check("variable_w_dead", driver.code, "c")
  }
  test("variable_w") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        x = 3
        x = 4
        printf("%d\n", x)
      }
    }
    check("variable_w", driver.code, "c")
  }

  test("variable_rw") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val y: Rep[Int] = x
        x = 3
        val z: Rep[Int] = x
        printf("%d\n", y + z)
      }
    }
    check("variable_rw", driver.code, "c")
  }

  test("variable_while_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          x += 1
        }
      }
    }
    check("variable_while_dead", driver.code, "c")
  }

  test("variable_while") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          x += 1
        }
        printf("%d\n", x)
      }
    }
    check("variable_while", driver.code, "c")
  }
  test("variable_if_nested") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        var y = 0
        if (arg > 10) {
          if (arg < 20) {
            x += 1
            y += 4
          }
        }
        printf("%d\n", x)
      }
    }
    check("variable_if_nested", driver.code, "c")
  }
  test("variable_if_nested_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        if (arg > 10) {
          if (arg < 20) x += 1
        }
      }
    }
    check("variable_if_nested_dead", driver.code, "c")
  }
}
