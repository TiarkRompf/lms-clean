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

  test("variable_while_dead1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          x += 1
        }
      }
    }
    check("variable_while_dead1", driver.code, "c")
  }

  test("variable_while_dead2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val a = NewArray[Int](10)
        while (x < 10) {
          a(x) = x
          x += 1
        }
      }
    }
    check("variable_while_dead2", driver.code, "c")
  }

  test("variable_while1") {
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
    check("variable_while1", driver.code, "c")
  }

  test("variable_while2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          printf("%d\n", x)
          x += 1
        }
      }
    }
    check("variable_while2", driver.code, "c")
  }

  test("variable_while3") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          printf("A\n")
          x += 1
        }
      }
    }
    check("variable_while3", driver.code, "c")
  }

  test("variable_while4") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        var y = 0
        var z = 0
        var t = 6
        while (x < 10) {
          z += y
          y += 1
          t += z
          x += 1
        }
        printf("%d\n", z)
      }
    }
    check("variable_while4", driver.code, "c")
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

  test("variable_write_after_read") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        printf("%d\n", x)
        if (x > 0)
          x += 1
        else printf("%d\n", x)
      }
    }
    check("variable_write_after_read", driver.code, "c")
  }

  test("variable_while_deps_read") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val y: Rep[Int] = x
        if (x < 50) {
          while (x < 10) {
            printf("%d\n", x)
            x += 1
          }
          printf("%d\n", y)
        }
        val z: Rep[Int] = x
        x += 1
        printf("%d\n", z)
        printf("%d\n", x)
      }
    }
    check("variable_while_deps_read", driver.code, "c")
  }
}
