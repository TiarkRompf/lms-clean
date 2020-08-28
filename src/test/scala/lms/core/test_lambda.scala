package lms
package core

import lms.core.stub._
import lms.macros.SourceContext

class LambdaTest extends TutorialFunSuite {
  val under = "backend/"

  test("bad_if_freq") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var a = arg + 10
        val a1 = a + 5
        val a2 = a - 4
        val s = if (arg > 20) a1 else a2
        printf("%d", s)
      }
    }
    System.out.println(indent(driver.code))
  }

  test("lambda_nested_if") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        def then_br(ss: Rep[Int]): Rep[Int] = {
          ss + 10
        }
        def else_br(ss: Rep[Int]): Rep[Int] = {
          ss * 2
        }
        val rep_then_br: Rep[Int => Int] = fun(then_br)
        val rep_else_br: Rep[Int => Int] = fun(else_br)
        val s = if (arg > 10) {
           rep_then_br(arg) - rep_else_br(arg)
        } else {
           rep_then_br(arg) + rep_else_br(arg)
        }
        printf("%d", s)
      }
    }
    System.out.println(indent(driver.code))
  }

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

  // Test for lambdas returned in conditionals
  test("returned_lambda_0") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = if (arg == 0) {
          fun { (n: Rep[Int]) => 1 }
        } else {
          fun { (n: Rep[Int]) => 2 }
        }
        println(f(arg))
      }
    }
    check("returned_lambda_0", driver.code, "scala")
  }

  // Test for lambdas returned in conditionals with effects on parameters
  test("returned_lambda_1") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = if (arg == 0) {
          fun { (a: Rep[Array[Int]]) => a(0) = 8; 1 }
        } else {
          fun { (a: Rep[Array[Int]]) => a(0) = 9; 2 }
        }
        val arr = NewArray[Int](10)
        f(arr)
        printf("%d %d", arr(0), arr(1))
      }
    }
    check("returned_lambda_1", driver.code, "scala")
  }

  // Test for lambdas returned by lambdas
  test("curried_lambda_0") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = fun { (n: Rep[Int]) =>
          fun { (m: Rep[Int]) => n + m }
        }
        printf("%d", f(arg)(arg+1))
      }
    }
    check("curried_lambda_0", driver.code, "scala")
  }

  // Test for lambdas returned by lambdas with effect on global variables
  test("curried_lambda_1") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 1
        val f = fun { (n: Rep[Int]) =>
          x = 1
          fun { (m: Rep[Int]) =>
            y = 2
            m + n
          }
        }
        printf("%d, %d, %d", f(arg)(arg), x, y)
      }
    }
    check("curried_lambda_1", driver.code, "scala")
  }

  // Test for lambdas returned by lambdas returned by lambdas with effect
  // on global variables
  test("curried_lambda_2") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 1
        var z = 2
        val f = fun { (n: Rep[Int]) =>
          x = 1
          fun { (m: Rep[Int]) =>
            y = 2
            fun { (x: Rep[Int]) =>
              z = 3
              n + m + x
            }
          }
        }
        printf("%d %d %d %d", f(arg)(arg)(arg), x, y, z)
      }
    }
    check("curried_lambda_2", driver.code, "scala")
  }

  // Test for lambdas returned by lambdas returned by lambdas with effect
  // on input variables
  test("curried_lambda_array_0") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val arr = NewArray[Int](10)
        val arr2 = NewArray[Int](5)
        val f = fun { (n: Rep[Array[Int]]) =>
          n(0) = 1
          fun { (n: Rep[Array[Int]]) =>
            n(1) = 2
            fun { (n: Rep[Array[Int]]) =>
              n(2) = 3
            }
          }
        }
        f(arr)(arr)(arr2)
        printf("%d, %d, %d, %d", arr(0), arr(1), arr2(0), arr2(2))
      }
    }
    check("curried_lambda_array_0", driver.code, "scala")
  }

  // Test for nested function application
  test("nested_application") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 1
        val f = fun { (n: Rep[Int]) => x = 1; n + 1}
        val g = fun { (n: Rep[Int]) => y = 3; n + 2}
        val res = f(g(arg))
        printf("%d, %d, %d", x, y, res)
      }
    }
    check("nested_application", driver.code, "scala")
  }

  test("lambda_closure") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 100
        val f = fun( { (n: Rep[Int]) =>
          x = 50
          NewArray[Int](n)
        })
        val arr = f(2)
        arr(0) = 1
        println(arr(1))
        println(x)
      }
    }
    check("lambda_closure", driver.code, "scala")
  }

  // Test for closed variables (outer scope is stage-level function)
  test("lambda_closure_1") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        def get_fun() = {
          var x = 100
          val f = fun({ (n: Rep[Int]) =>
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
        println(arr(0))
        println(arr(1))
        println(arr(2))
      }
    }
    check("lambda_closure_1", driver.code, "scala")
  }

  // Test for closed variables (outer scope is conditional)
  test("lambda_closure_2") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = if (arg == 0) {
          var x = 100
          fun({ (n: Rep[Int]) =>
            val a = NewArray[Int](n)
            a(0) = x
            a
          })
        } else {
          var y = 40
          fun({ (n: Rep[Int]) =>
            val b = NewArray[Int](n)
            b(0) = y
            b
          })
        }
        val arr = f(3)
        arr(1) = 70
        println(arr(0))
        println(arr(1))
        println(arr(2))
      }
    }
    check("lambda_closure_2", driver.code, "scala")
  }

  // FIXME(feiw): need to add more cases in getFunctionLatentEffect (backend.scala line 385)
  // test("lambda_closure_3") {
  //   val driver = new DslDriver[Int, Unit] with lms.collection.immutable.TupleOps {
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

  test("recursion_lambda_forward_effect") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(a: Rep[Int]) = {
        lazy val f: Rep[(Int, Array[Int]) => Unit] = fun { (c: Rep[Int], x: Rep[Array[Int]]) =>
          printf("%d\n", x(c))
          if (c > 0) {
            val na = Array(c,c,c,c,c)
            na(c-1) = 100 // this line of code should not be removed!
            f(c-1, na)
          }
        }
        val arr = Array(1,1,1,1,1)
        f(3, arr)
      }
    }
    checkOut("recursion_lambda_forward_effect", "scala", {
      println(driver.code)
      println("// output:")
      driver.eval(7)
    })
  }

  test("mutual_recursion") {
    val driver = new DslDriver[Int,Unit] {
      @virtualize
      def snippet(a: Rep[Int]) = {
        lazy val odd: Rep[Int=>Boolean] = fun { (x: Rep[Int]) =>
          if (x == 1) true else even(x - 1)
        }
        lazy val even: Rep[Int=>Boolean] = fun { (x: Rep[Int]) =>
          if (x == 0) true else odd(x - 1)
        }
        printf("%d", even(a))
      }
    }
    check("mutual_recursion", driver.code, "scala")
  }

  test("be_careful_with_lazy") {
    val driver = new DslDriver[Int, Unit] {
      @virtualize
      def snippet(a: Rep[Int]) = {
        // if the fun1 is `lazy val` the generated code has
        // 2 instances of the fun2 code, since the `lazy val`
        // semantics changes the canonicanize result of `fun2`
        val fun1 = fun { (x: Rep[Int], y: Rep[Int]) =>
          x + y
        }
        def fun2: Rep[Int => Int] = fun { (x: Rep[Int]) =>
          if (x == 1) 1 else fun1(fun2(x/2), fun2(x/2))
        }
        printf("%d %d", fun2(a), fun2(a))
      }
    }
    check("be_careful_with_lazy", driver.code, "scala")
  }
}
