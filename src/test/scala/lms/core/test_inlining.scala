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

  test("branch_blocks_interfer") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val res = arg - 4
        val y = if (arg > 4) res * 3 else res * 4
        printf("%d\n", y)
      }
    }
    val src = driver.code
    checkOut("branch_blocks_interfer", "c", {
      println(src)
      println("// output:")
      driver.eval(7)
      driver.eval(3)
    })
  }

  test("cond_branch_blocks_interfer") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val res = arg - 4
        val y = if (if (arg > 0) res > 0 else res < 3) res else res + 1
        printf("%d\n", y)
      }
    }
    val src = driver.code
    checkOut("cond_branch_blocks_interfer", "c", {
      println(src)
      println("// output:")
      driver.eval(7)
      driver.eval(3)
    })
  }

  test("variable_inlining") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        while (x < arg) {
          val y: Rep[Int] = x
          x += 1
          printf("%d\n", y)
        }
        var z = 0
        while (z < arg) {
          val y: Rep[Int] = z
          printf("%d\n", y)
          z += 1
        }
      }
    }
    val src = driver.code
    checkOut("variable_inlining", "c", {
      println(src)
      println("// output:")
      driver.eval(4)
    })
  }
}
