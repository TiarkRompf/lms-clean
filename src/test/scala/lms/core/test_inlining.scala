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

  test("recursion_1") {
    val driver = new DslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
          if (x > 0) x * f(x - 1)
          else 1
        }
        var x = 3
        f(x)
      }
    }
    val src = driver.code
    driver.precompile
    checkOut("recursion_1", "scala", {
      println(src)
      println("// output:")
      driver.eval(4)
      driver.eval(5)
    })
  }

  test("recursion_2") {
    val driver = new DslDriver[Int,Unit] {

      @virtualize
      def snippet(a: Rep[Int]) = {
        var i = 0
        while (i < 10) {
          lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
            if (x > 0) x * f(x - a)
            else 1
          }
          f(i)
          i = i + 1
        }
      }
    }
    val src = driver.code
    driver.precompile
    checkOut("recursion_2", "scala", {
      println(src)
      println("// output:")
      driver.eval(4)
      driver.eval(5)
    })
  }

  test("recursion_3") {
    val driver = new DslDriver[Int,Unit] {

      @virtualize
      def snippet(a: Rep[Int]) = {
        var i = 0
        while (i < 10) {
          var j = i
          lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
            if (x > 0) j * f(x - a)
            else 1
          }
          f(i)
          i = i + 1
        }
      }
    }
    val src = driver.code
    driver.precompile
    checkOut("recursion_3", "scala", {
      println(src)
      println("// output:")
      driver.eval(4)
      driver.eval(5)
    })
  }

}
