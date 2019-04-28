package lms
package core

import stub._
import macros.SourceContext
import utils.time

import java.io.File
import java.io.PrintWriter

import scala.util.continuations._
import scala.util.continuations

abstract class CPSDslDriver[A:Manifest,B:Manifest] extends DslSnippet[A,B] with DslImpl with CompileScala {
  Adapter.typeMap = new scala.collection.mutable.HashMap[lms.core.Backend.Exp, Manifest[_]]()
  Adapter.funTable = Nil

  def extra(x: A): Unit = {
    val source = new java.io.ByteArrayOutputStream()
    def show(note: String)(f: => Unit) = {
      System.out.println(note)
      System.out.println(utils.captureOut(f))
    }
    show("// Generic CodeGen") {
      (new GenericCodeGen)(g)
    }
    show("// Scala CodeGen") {
      (new ScalaCodeGen)(g)
    }
    show("// CPS Scala CodeGen") {
      (new CPSScalaCodeGen)(g)
    }
  }

  // CPS CodeGen
  lazy val g: Graph = time("staging") {Adapter.program(Adapter.g.reify(x => Unwrap(wrapper(Wrap[A](x)))))}

  lazy val code: String = utils.captureOut{ (new CPSScalaCodeGen).emitAll(g)(manifest[A],manifest[B]) }

  def eval(x: A): B = {val f1 = f; time("eval")(f1(x))}

  lazy val f = { val c1 = code; time("scalac") { Global.sc.compile[A,B]("Snippet", c1, Nil) }}

  // CPS Transformation
  val transformer = new CPSTransformer { g = Adapter.mkGraphBuilder() }

  lazy val tg: Graph = time("transforming") { transformer.transform(g) }

  lazy val code2: String = utils.captureOut{ (new ScalaCodeGen).emitAll(tg)(manifest[A],manifest[B]) }

  def eval2(x: A): B = {val f1 = f2; time("eval")(f1(x))}

  lazy val f2 = { val c1 = code2; time("scalac") { Global.sc.compile[A,B]("Snippet", c1, Nil) }}

}


class CPSTest extends TutorialFunSuite {
  val under = "cps/"

  test("dc") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val x1 = reset1 {
          shift1[Int, Int]{ k =>
            2 * k(arg)
          } + shift1[Int, Int] { k =>
            3 * k(arg)
          } + 4
        }
        x1 * arg
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = {
        arg * reset {
          shift { (k: Int => Int) =>
            2 * k(arg)
          } + shift { (k: Int => Int) =>
            3 * k(arg)
          } + 4
        }
      }
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("dc", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("dcTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("dcIf") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val x1 = reset1 {
          1 + shift1[Int, Int] { k =>
            3 + (if (arg > 5) k(k(arg))
                 else k(arg) + 5)
          } * 2
        } + 4
        x1 + 10
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = {
        val x1 = reset {
          1 + shift { (k: Int => Int) =>
            3 + (if (arg > 5) k(k(arg))
                 else k(arg) + 5)
          } * 2
        } + 4
        x1 + 10
      }
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("dcIf", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("dcIfTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("ifDc") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val x1 = reset1 {
          def s(x: Rep[Int]) = shift1[Int, Int] { k => k(k(x)) + 7 }
          if (arg > 5) {
            s(arg - 5) * 3
          } else {
            s(arg + 5) * 2
          } + 2
        } + 4
        x1 + 10
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = {
        val x1 = reset {
          def s(x: Int) = shift{ (k: Int => Int) => k(k(x)) + 7 }
          if (arg > 5) {
            s(arg - 5) * 3
          } else {
            s(arg + 5) * 2
          } + 2
        } + 4
        x1 + 10
      }
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("ifDc", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("ifDcTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("ifDc1") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val x1 = reset1 {
          def s(x: Rep[Int]) = shift1[Int, Int] { k => k(k(x)) + 7 }
          if (arg > 5) {
            s(arg - 5) * 3
          } else {
            arg + 100
          } + 2
        } + 4
        x1 + 10
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = {
        val x1 = reset {
          def s(x: Int) = shift{ (k: Int => Int) => k(k(x)) + 7 }
          if (arg > 5) {
            s(arg - 5) * 3
          } else {
            arg + 100
          } + 2
        } + 4
        x1 + 10
      }
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("ifDc1", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("ifDc1Trans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("dcWhile") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          shift1[Int, Int] { k =>
            var i = 0
            var res = 0
            while (i < 5) {
              res = k(res) + k(arg) + k(i)
              i = i + 1
            }
            res
          } * 2
        } + 4
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = reset {
        shift { (k: Int => Int) =>
          var i = 0
          var res = 0
          while (i < 5) {
            res = k(res) + k(arg) + k(i)
            i = i + 1
          }
          res
        } * 2
      } + 4
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("dcWhile", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("dcWhileTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("whileDc") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          var i = 0
          var res = 0
          while (i < 5) {
            shift1[Int, Int] { k =>
              res = k(res) + k(arg) + k(i)
              res
            } * 2
            i = i + 1
          }
          res
        } + 4
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = reset {
        var i = 0
        var res = 0
        while (i < 5) {
          shift { (k: Int => Int) =>
            res = k(res) + k(arg) + k(i)
            res
          } * 2
          i = i + 1
        }
        res
      } + 4
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("whileDc", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("whileDcTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("whileDc2") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        var i = 0
        var res = 0
        while (i < 5) {
          i = i + 1
          reset1 {
            shift1[Int, Int] { k =>
              res = k(res) + k(arg) + k(i)
              res
            } * 2
          } + 4
          ()
        }
        res
      }
    }
    // test by running
    for (arg <- 0 until 10) {
      val expect = {
        var i = 0
        var res = 0
        while (i < 5) {
          i = i + 1
          reset {
            shift { (k: Int => Int) =>
              res = k(res) + k(arg) + k(i)
              res
            } * 2
          } + 4
        }
        res
      }
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("whileDc2", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("whileDc2Trans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("dcLambda") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          shift1[Int, Int] { k =>
            val double = fun { (x: Rep[Int]) => x * 2 }
            double(k(double(k(arg))))
          } + 2
        } + 5
      }
    }
    // test by running
    for(arg <- 0 until 10) {
      val expect = reset {
        shift { (k: Int => Int) =>
          val double = (x: Int) => x * 2
          double(k(double(k(arg))))
        } + 2
      } + 5
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("dcLambda", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("dcLambdaTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("lambdaDc") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          val f = fun { (x: Rep[Int]) =>
            shift1[Int, Int] { k =>
              k(x) + k(arg)
            }
          }
          f(arg + 2)
        } + 5
      }
    }
    // test by running
    for(arg <- 0 until 10) {
      val expect = reset {
        val f = (x: Int) => shift { (k: Int => Int) =>
          k(x) + k(arg)
        }
        f(arg + 2)
      } + 5
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("lambdaDc", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("lambdaDcTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("lambdaDc2") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val f = fun { (x: Rep[Int]) =>
          reset1 {
            shift1[Int, Int] { k =>
              k(x) + k(arg)
            } * 2
          } + 5
        }
        f(arg + 2)
      }
    }
    // test by running
    for(arg <- 0 until 10) {
      val expect = {
        val f = (x: Int) => {
          reset {
            shift { (k: Int => Int) =>
              k(x) + k(arg)
            } * 2
          } + 5
        }
        f(arg + 2)
      }
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("lambdaDc2", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("lambdaDc2Trans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("DcRecursion") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          shift1[Int, Int] { k =>
            lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
              if (x > 0) k(x) * f(x - 1)
              else k(x)
            }
            f(arg)
          } * 2 + 3
        } + 10
      }
    }
    // test by running
    for (arg <- 1 until 5) {
      val expect = reset {
        shift { (k: Int => Int) =>
          def f(x: Int): Int = if (x > 0) k(x) * f(x - 1) else k(x)
          f(arg)
        } * 2 + 3
      } + 10
      assert(driver.eval(arg) ==  expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("DcRecursion", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("DcRecursionTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("recursionDc") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
          reset1 {
            shift1[Int, Int] { k =>
              if (x > 0) k(x) * f(x - 1) else k(x)
            } * 2 + 3
          } + 10
        }
        f(arg)
      }
    }
    // test by running
    for (arg <- 1 until 5) {
      val expect = {
        def f(x: Int): Int = reset {
          shift { (k: Int => Int) =>
            if (x > 0) k(x) * f(x - 1) else k(x)
          } * 2 + 3
        } + 10
        f(arg)
      }
      assert(driver.eval(arg) ==  expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("recursionDc", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("RecursionDcTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  // An example of nested shift. the shift/reset compile plugin disallows it!
  // test("recursionDc") {
  //   val driver = new CPSDslDriver[Int,Int] {

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Int] = {
  //       reset1 {
  //         lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
  //           shift1[Int, Int] { k =>
  //             if (x > 0) k(x) * f(x - 1) else k(x)
  //           } * 2 + 3
  //         }
  //         f(arg)
  //       } + 10
  //     }
  //   }
  //   // test by running
  //   for (arg <- 1 until 10) {
  //     val expect = reset {
  //       def f(x: Int): Int = shift { (k: Int => Int) =>
  //         if (x > 0) k(x) * f(x - 1) else k(x)
  //       } * 2 + 3
  //       f(arg)
  //     } + 10
  //     assert(driver.eval(arg) ==  expect)
  //     assert(driver.eval2(arg) == expect)
  //   }
  //   // test source
  //   val src = driver.code
  //   checkOut("recursionDc", "scala", {
  //     println(src)
  //     println("// output:")
  //   })
  //   val src2 = driver.code2
  //   checkOut("recursionDcTrans", "scala", {
  //     println(src2)
  //     println("// output:")
  //   })
  // }

  test("simple") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(a: Rep[Int]): Rep[Int] = {
        val x = a + a
        val y = x * a
        val z = y / y
        z
      }
    }
    // test by running
    for (i <- 1 until 10) {
      assert(driver.eval(i) == 1)
      assert(driver.eval2(i) == 1)
    }
    // test source
    val src = driver.code
    checkOut("simple", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("simpleTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("if") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(a: Rep[Int]): Rep[Int] = {
        val x = a + a
        if (x > 2) {
          x * a + a
        } else {
          x / a - a
        }
      }
    }
    // test by running
    for (i <- 1 until 10) {
      val expect = if (i > 1) 2*i*i+i else 2-i
      assert(driver.eval(i) ==  expect)
      assert(driver.eval2(i) ==  expect)
    }
    // test source
    val src = driver.code
    checkOut("if", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("ifTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("while") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(a: Rep[Int]): Rep[Int] = {
        var x = a + a
        var res = 0
        while(x > 0) {
          res = res + x
          x = x - 1
        }
        res
      }
    }
    // test by running
    for (i <- 1 until 11) {
      assert(driver.eval(i) == (2*i + 1)*i )
      assert(driver.eval2(i) == (2*i + 1)*i )
    }
    // test source
    val src = driver.code
    checkOut("while", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("whileTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("lambda") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(a: Rep[Int]): Rep[Int] = {
        val double = fun{ (x: Rep[Int]) => x * 2 }
        double(double(a))
      }
    }
    // test by running
    for(i <- 0 until 10) {
      assert(driver.eval(i) == i*4)
      assert(driver.eval2(i) == i*4)
    }
    // test source
    val src = driver.code
    checkOut("lambda", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("lambdaTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("recursion") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
          if (x > 0) x * f(x - 1)
          else 1
        }
        f(arg)
      }
    }
    // test by running
    for (i <- 1 until 5) {
      val expect = (1 until i+1).toSeq.fold(1:Int){_*_}
      assert(driver.eval(i) ==  expect)
      assert(driver.eval2(i) == expect)
    }
    // test source
    val src = driver.code
    checkOut("recursion", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("recursionTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("recursionWhile") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        var i = 0
        var res = 0
        while (i < 10) {
          lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
            if (x > 0) x * f(x - arg)
            else 1
          }
          res = f(i)
          i = i + 1
        }
        res
      }
    }
    // test by running
    for (arg <- 1 until 5) {
      val expect = (9 until 0 by (-arg)).toSeq.fold(1:Int)(_*_)
      assert(driver.eval(arg) == expect)
      assert(driver.eval2(arg) == expect)
    }
    // test source
    val src = driver.code
    checkOut("recursionWhile", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("recursionWhileTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("ifWhile") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val x = arg + arg
        var i = 0
        var res = 0
        if (x > 10) {
          while (i < x - 10) {
            res = res + i
            i = i + 1
          }
          res
        } else {
          while (i < x + 10) {
            res = res + i
            i = i + 1
          }
          res
        }
      }
    }
    // test by running
    for (arg <- 1 until 10) {
      val x = arg * 2
      val bound = if (x > 10) x - 10 else x + 10
      val result = (bound - 1) * bound / 2
      assert(driver.eval(arg) == result)
      assert(driver.eval2(arg) == result)
    }
    // test source
    val src = driver.code
    checkOut("ifWhile", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("ifWhileTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }

  test("whileLambda") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val double = fun{(x: Rep[Int]) => 2 * x}
        var i = 0
        var res = 0
        while(i < arg) {res = res + double(i); i = i + 1}
        res
      }
    }
    // test by running
    for(arg <- 1 until 10) {
      assert(driver.eval(arg) == (arg - 1) * arg)
      assert(driver.eval2(arg) == (arg - 1) * arg)
    }
    // test source
    val src = driver.code
    checkOut("whileLambda", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("whileLambdaTrans", "scala", {
      println(src2)
      println("// output:")
    })
  }
}
