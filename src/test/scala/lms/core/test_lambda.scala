package lms
package core

import lms.core.stub._
import lms.macros.SourceContext

class LambdaTest extends TutorialFunSuite {
  val under = "backend/"

  test("lambda_01") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          arr(0) = 5
        }
        val arr = NewArray[Int](1)
        f(arr)
        println(arr(0))
      }
    }
    check("lambda_01", driver.code, "scala")
  }

  test("lambda_02") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          arr(0)
        }
        val arr = NewArray[Int](1)
        arr(0) = 5
        println(f(arr))
      }
    }
    check("lambda_02", driver.code, "scala")
  }

  test("lambda_03_1") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          println("B")
        }
        val arr = NewArray[Int](1)
        f(arr)
        println("A")
      }
    }
    check("lambda_03", driver.code, "scala")
  }

  test("lambda_03_2") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          println("B")
        }
        val arr = NewArray[Int](1)
        arr(0) = 5
        f(arr)
        println("A")
      }
    }
    check("lambda_03", driver.code, "scala")
  }

  test("lambda_04") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          arr(0) = 5
        }
        val arr = NewArray[Int](1)
        f(arr)
        println("A")
      }
    }
    check("lambda_04", driver.code, "scala")
  }

  test("lambda_05") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (n: Rep[Int]) =>
          NewArray[Int](n)
        }
        val arr = f(1)
        arr(0) = 1
        println(arr(0))
      }
    }
    check("lambda_05", driver.code, "scala")
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
    checkOut("recursion_1", "scala", {
      println(src)
      println("// output:")
      utils.devnull(driver.precompile)
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
          println(f(i))
          i = i + 1
        }
      }
    }
    val src = driver.code
    checkOut("recursion_2", "scala", {
      println(src)
      println("// output:")
      utils.devnull(driver.precompile)
      driver.eval(1)
      driver.eval(2)
    })
  }

  test("recursion_3") {
    val driver = new DslDriver[Int,Unit] {

      @virtualize
      def snippet(a: Rep[Int]) = {
        var i = 0
        while (i < 10) {
          val j = i
          lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
            if (x > 0) j * f(x - a)
            else 1
          }
          println(f(i))
          i = i + 1
        }
      }
    }
    val src = driver.code
    checkOut("recursion_3", "scala", {
      println(src)
      println("// output:")
      utils.devnull(driver.precompile)
      driver.eval(1)
      driver.eval(2)
    })
  }
}
