package lms
package core

import stub._
import macros.SourceContext
import utils.time

import java.io.File
import java.io.PrintWriter

abstract class CPSDslDriver[A:Manifest,B:Manifest] extends DslSnippet[A,B] with DslImpl with CompileScala {
  Adapter.typeMap = new scala.collection.mutable.HashMap[lms.core.Backend.Exp, Manifest[_]]()
  Adapter.funTable = Nil
  var filename: String = _

  lazy val g = time("staging") {Adapter.program(Adapter.g.reify(x => Unwrap(wrapper(Wrap[A](x)))))}

  def extra(x: A, k: Int): Unit = {
    val source = new java.io.ByteArrayOutputStream()
    // var g = time("staging") {Adapter.program(Adapter.g.reify(x => Unwrap(wrapper(Wrap[A](x)))))}

    def show(note: String)(f: => Unit) = {
      System.out.println(note)
      System.out.println(utils.captureOut(f))
    }

    show("// Generic CodeGen") {
      (new CodeGen)(g)
    }
    show("// Scala CodeGen") {
      (new ScalaCodeGen)(g)
    }
    show("// CPS Scala CodeGen") {
      (new CPSScalaCodeGen)(g)(k)
    }
  }

  def code(k: Int): String = {
    val temp = utils.captureOut{ (new CPSScalaCodeGen).emitAll(g, k)(manifest[A],manifest[B]) }
    val outFile = new PrintWriter(new File(filename))
    outFile.println(temp)
    outFile.flush()
    temp
  }

  def eval(x: A, k: Int): B = {val f1 = f(k); time("eval")(f1(x))}

  def f(k: Int) = { val c1 = code(k); time("scalac") { Global.sc.compile[A,B]("Snippet", c1, Nil) }}

}

class CPSTest extends TutorialFunSuite {
  val under = "cps/"

  test("simple") {
    val driver = new CPSDslDriver[Int,Int] {
      filename = "/tmp/simple.scala"

      @virtualize
      def snippet(a: Rep[Int]): Rep[Int] = {
        val x = a + a
        val y = x * a
        val z = y / y
        z
      }
    }
    for (i <- 0 until 10) {
      driver.eval(i+1, 1)
    }
  }

  test("if") {
    val driver = new CPSDslDriver[Int, Int] {
      filename = "/tmp/if.scala"

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
    for (i <- 1 until 10) {
      driver.eval(i, if ((i+i)>2){(i+i)*i+i}else{(i+i)/i-i} )
    }
  }

  test("while") {
    val driver = new CPSDslDriver[Int,Int] {
      filename = "/tmp/while.scala"

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
    for (i <- 1 until 11) {
      driver.eval(i, (2*i + 1)*i )
    }
  }

  test("lambda") {
    val driver = new CPSDslDriver[Int, Int] {
      filename = "/tmp/lambda.scala"

      @virtualize
      def snippet(a: Rep[Int]): Rep[Int] = {
        val double = fun{ (x: Rep[Int]) => x * 2 }
        double(double(a))
      }
    }
    for(i <- 0 until 10) {
      driver.eval(i, i*4)
    }
  }

  test("recursion") {
    val driver = new CPSDslDriver[Int,Int] {
      filename = "/tmp/recursion.scala"

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        lazy val f: Rep[Int => Int] = fun { (x: Rep[Int]) =>
          if (x > 0) x * f(x - 1)
          else 1
        }
        f(arg)
      }
    }
    for (i <- 1 until 5) {
      driver.eval(i, (1 until i+1).toSeq.fold(1:Int){_*_} )
    }
  }

  test("recursionWhile") {
    val driver = new CPSDslDriver[Int,Int] {
      filename = "/tmp/recursionWhile.scala"

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
    for (arg <- 1 until 5)
      driver.eval(arg, (9 until 0 by (-arg)).toSeq.fold(1:Int)(_*_))
  }

  test("ifWhile") {
    val driver = new CPSDslDriver[Int,Int] {
      filename = "/tmp/ifWhile.scala"

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
    for (arg <- 1 until 10) {
      val x = arg * 2
      val bound = if (x > 10) x - 10 else x + 10
      val result = (bound - 1) * bound / 2
      driver.eval(arg, result)
    }
  }

  test("whileLambda") {
    val driver = new CPSDslDriver[Int, Int] {
      filename = "/tmp/whileLambda.scala"

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        val double = fun{(x: Rep[Int]) => 2 * x}
        var i = 0
        var res = 0
        while(i < arg) {res = res + double(i); i = i + 1}
        res
      }
    }
    for(arg <- 1 until 10) {
      driver.eval(arg, (arg - 1) * arg)
    }
  }
}
