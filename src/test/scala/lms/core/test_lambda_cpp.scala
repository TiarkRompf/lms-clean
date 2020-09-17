package lms
package core

import lms.core.stub._
import lms.macros.SourceContext

class CPPLambdaTest extends TutorialFunSuite {
  val under = "backend/"

  test("lambda_01") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          arr(0) = 5
        }
        val arr = NewArray[Int](1)
        f(arr)
        printf("%d\n", arr(0))
      }
    }
    check("lambda_01", driver.code, "c")
    val actual = lms.core.utils.captureOut(driver.eval(100))
    assert(actual == "5\n")
  }

  test("lambda_02") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          arr(0)
        }
        val arr = NewArray[Int](1)
        arr(0) = 5
        printf("%d\n", f(arr))
      }
    }
    check("lambda_02", driver.code, "c")
    val actual = lms.core.utils.captureOut(driver.eval(100))
    assert(actual == "5\n")
  }

  test("lambda_03_1") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          printf("B")
        }
        val arr = NewArray[Int](1)
        f(arr)
        printf("A")
      }
    }
    check("lambda_03", driver.code, "c")
    val actual = lms.core.utils.captureOut(driver.eval(100))
    assert(actual == "BA\n")
  }

  test("lambda_03_2") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          printf("B")
        }
        val arr = NewArray[Int](1)
        arr(0) = 5
        f(arr)
        printf("A")
      }
    }
    check("lambda_03", driver.code, "c")
    val actual = lms.core.utils.captureOut(driver.eval(100))
    assert(actual == "BA\n")
  }

  test("lambda_04") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (arr: Rep[Array[Int]]) =>
          arr(0) = 5
        }
        val arr = NewArray[Int](1)
        f(arr)
        printf("A")
      }
    }
    check("lambda_04", driver.code, "c")
    val actual = lms.core.utils.captureOut(driver.eval(100))
    assert(actual == "A\n")
  }

  test("lambda_05") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (n: Rep[Int]) =>
          NewArray[Int](n)
        }
        val arr = f(1)
        arr(0) = 1
        printf("%d\n", arr(0))
      }
    }
    check("lambda_05", driver.code, "c")
    val actual = lms.core.utils.captureOut(driver.eval(100))
    assert(actual == "1\n")
  }

  // Test for lambdas returned in conditionals
  test("returned_lambda_0") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = if (arg == 0) {
          fun { (n: Rep[Int]) => 1 }
        } else {
          fun { (n: Rep[Int]) => 2 }
        }
        printf("%d\n", f(arg))
      }
    }
    check("returned_lambda_0", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(0)) == "1\n")
    assert(lms.core.utils.captureOut(driver.eval(1)) == "2\n")
  }

  // Test for lambdas returned in conditionals with effects on parameters
  test("returned_lambda_1") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = if (arg == 0) {
          fun { (a: Rep[Array[Int]]) => a(0) = 8; 1 }
        } else {
          fun { (a: Rep[Array[Int]]) => a(0) = 9; 2 }
        }
        val arr = NewArray[Int](10)
        printf("%d", f(arr))
        printf(", %d\n", arr(0))
      }
    }
    check("returned_lambda_1", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(0)) == "1, 8\n")
    assert(lms.core.utils.captureOut(driver.eval(1)) == "2, 9\n")
  }

  // Test for lambdas returned by lambdas
  test("curried_lambda_0") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (n: Rep[Int]) =>
          fun("=", { (m: Rep[Int]) => n + m })
        }
        printf("%d\n", f(arg)(arg+1))
      }
    }
    check("curried_lambda_0", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "5\n")
  }

  // Test for lambdas returned by lambdas with effect on global variables
  test("curried_lambda_1") {
    val driver = new DslDriverCPP[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 1
        val f = fun("&", { (n: Rep[Int]) =>
          x = 1
          fun (Captures.RefExcept(n), { (m: Rep[Int]) =>
            y = 2
            m + n
          })
        })
        printf("%d", f(arg)(arg))
        printf(", %d, %d\n", x, y)
      }
    }
    check("curried_lambda_1", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "4, 1, 2\n")
  }

  // Test for lambdas returned by lambdas returned by lambdas with effect
  // on global variables
  test("curried_lambda_2") {
    val driver = new DslDriverCPP[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 1
        var z = 2
        val f = fun("&", { (n: Rep[Int]) =>
          x = 1
          fun(Captures.RefExcept(n), { (m: Rep[Int]) =>
            y = 2
            fun(Captures.RefExcept(n, m), { (x: Rep[Int]) =>
              z = 3
              n + m + x
            })
          })
        })
        printf("%d", f(arg)(arg)(arg))
        printf(", %d, %d, %d\n", x, y, z)
      }
    }
    check("curried_lambda_2", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "6, 1, 2, 3\n")
  }

  // Test for lambdas returned by lambdas returned by lambdas with effect
  // on input variables
  test("curried_lambda_array_0") {
    val driver = new DslDriverCPP[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val arr = NewArray[Int](10)
        val arr2 = NewArray[Int](5)
        // NOTE(feiw): it might seem strange at first glance that we need `&` capture.
        // it is because the code motion might hoist the inner functions to the top level,
        // which creates captures!
        val f = fun("&", { (n: Rep[Array[Int]]) =>
          n(0) = 1
          fun("=", { (n: Rep[Array[Int]]) =>
            n(1) = 2
            fun { (n: Rep[Array[Int]]) =>
              n(2) = 3
            }
          })
        })
        f(arr)(arr)(arr2)
        printf("%d, %d, %d\n", arr(0), arr(1), arr2(2))
      }
    }
    check("curried_lambda_array_0", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "1, 2, 3\n")
  }

  // Test for nested function application
  test("nested_application") {
    val driver = new DslDriverCPP[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 1
        val f = fun("&", { (n: Rep[Int]) => x = 1; n + 1})
        val g = fun("&", { (n: Rep[Int]) => y = 3; n + 2})
        val res = f(g(arg))
        printf("%d, %d, %d", x, y, res)
      }
    }
    check("nested_application", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "1, 3, 5\n")
  }

  test("lambda_closure") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 100
        val f = fun("&", { (n: Rep[Int]) =>
          x = 50
          NewArray[Int](n)
        })
        val arr = f(2)
        arr(0) = 1
        printf("%d, %d\n", arr(0), x)
      }
    }
    check("lambda_closure", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "1, 50\n")
  }

  // Test for closed variables (outer scope is stage-level function)
  test("lambda_closure_1") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        def get_fun() = {
          var x = 100
          val f = fun("&", { (n: Rep[Int]) =>
            val a = NewArray[Int](n)
            a(0) = x
            x = 20
            a
          })
          (f, x)
        }
        val (f, x) = get_fun()
        val arr = f(3)
        arr(1) = x
        printf("%d, %d, %d\n", arr(0), arr(1), x)
      }
    }
    check("lambda_closure_1", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "100, 20, 20\n")
  }

  // Test for closed variables (outer scope is conditional)
  test("lambda_closure_2") {
    val driver = new DslDriverCPP[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = if (arg == 0) {
          var x = 100
          fun("=", { (n: Rep[Int]) =>
            val a = NewArray[Int](n)
            a(0) = x
            a
          })
        } else {
          var y = 40
          fun("=", { (n: Rep[Int]) =>
            val b = NewArray[Int](n)
            b(0) = y
            b
          })
        }
        val arr = f(3)
        arr(1) = 70
        printf("%d, %d\n", arr(0), arr(1))
      }
    }
    check("lambda_closure_2", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "40, 70\n")
    assert(lms.core.utils.captureOut(driver.eval(0)) == "100, 70\n")
  }

  // FIXME(feiw): need tuple support from Guannan's PR
  // test("lambda_closure_3") {
  //   val driver = new DslDriver[Int, Unit] {
  //     @virtualize
  //     def snippet(arg: Rep[Int]) = {
  //       val f = fun { (n: Rep[Int]) =>
  //         var x = 0
  //         val g = fun { (m: Rep[Int]) => x = 1; n + m}
  //         (g, x)
  //       }
  //       val ff = f(arg)
  //       val res = ff._1(arg + 1)
  //       printf("%d, %d", ff._2, res)
  //     }
  //   }
  //   System.out.println(indent(driver.code))
  // }

  test("recursion_1") {
    val driver = new DslDriverCPP[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        lazy val f: Rep[Int => Int] = fun("&", { (x: Rep[Int]) =>
          if (x > 0) x * f(x - 1)
          else 1
        })
        var x = 3
        printf("%d\n", f(x))
      }
    }
    check("recursion_1", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "6\n")
  }

  test("recursion_2") {
    val driver = new DslDriverCPP[Int,Unit] {

      @virtualize
      def snippet(a: Rep[Int]) = {
        var i = 0
        while (i < 10) {
          lazy val f: Rep[Int => Int] = fun("&", { (x: Rep[Int]) =>
            if (x > 0) x * f(x - a)
            else 1
          })
          printf("%d, ", f(i))
          i = i + 1
        }
      }
    }
    check("recursion_2", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "1, 1, 2, 3, 8, 15, 48, 105, 384, 945, \n")
  }

  test("recursion_3") {
    val driver = new DslDriverCPP[Int,Unit] {

      @virtualize
      def snippet(a: Rep[Int]) = {
        var i = 0
        while (i < 10) {
          val j = i
          lazy val f: Rep[Int => Int] = fun("&", { (x: Rep[Int]) =>
            if (x > 0) j * f(x - a)
            else 1
          })
          printf("%d, ", f(i))
          i = i + 1
        }
      }
    }
    check("recursion_3", driver.code, "c")
    assert(lms.core.utils.captureOut(driver.eval(2)) == "1, 1, 2, 9, 16, 125, 216, 2401, 4096, 59049, \n")
  }

  // FIXME(feiw) so far the codegen replaces the lambdaforward symbol with lambda symbol.
  // it breaks at mutual recursive closures
  // test("mutual_recursion") {
  //   val driver = new DslDriverCPP[Int,Unit] {
  //     @virtualize
  //     def snippet(a: Rep[Int]) = {
  //       lazy val odd: Rep[Int=>Boolean] = fun("&", { (x: Rep[Int]) =>
  //         if (x == 1) true else even(x - 1)
  //       })
  //       lazy val even: Rep[Int=>Boolean] = fun("&", { (x: Rep[Int]) =>
  //         if (x == 0) true else odd(x - 1)
  //       })
  //       printf("%d\n", even(a))
  //     }
  //   }
  //   check("mutual_recursion", driver.code, "c")
  //   assert(lms.core.utils.captureOut(driver.eval(2)) == "1\n")
  //   assert(lms.core.utils.captureOut(driver.eval(9)) == "0\n")
  // }
}
