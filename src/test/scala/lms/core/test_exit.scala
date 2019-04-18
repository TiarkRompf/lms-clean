package lms
package core

import stub._
import macros.SourceContext

class ExitTest extends TutorialFunSuite {
  val under = "core/"

  test("exit_rep1") {
    val driver = new DslDriverC[Int,Unit] {

      def repExit(x: Rep[Int]): Rep[Unit] = Wrap[Unit](Adapter.g.reflectEffect("exit", Unwrap(x))(Adapter.CTRL))
      @virtualize
      def myassert(cond: Rep[Boolean], msg: String): Rep[Unit] = if (cond) { printf(msg); repExit(0) }

      @virtualize
      def snippet(arg: Rep[Int]) = {
        myassert(arg > 0, "fail")
      }
    }
    check("exit_rep1", driver.code, "c")
  }

  test("exit_rep2") {
    val driver = new DslDriverC[Int,Unit] {

      def repExit(x: Rep[Int]): Rep[Unit] = Wrap[Unit](Adapter.g.reflectEffect("exit", Unwrap(x))(Adapter.CTRL))
      @virtualize
      def myassert(cond: Rep[Boolean], msg: String): Unit = if (cond) { printf(msg); repExit(0) }

      @virtualize
      def snippet(arg: Rep[Int]) = {
        myassert(arg > 0, "fail")
      }
    }
    check("exit_rep2", driver.code, "c")
  }

  test("exit1") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def myassert(cond: Rep[Boolean], msg: String): Rep[Unit] = if (cond) { printf(msg); exit(0) }

      @virtualize
      def snippet(arg: Rep[Int]) = {
        myassert(arg > 0, "fail")
      }
    }
    check("exit1", driver.code, "c")
  }

  test("exit2") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def myassert(cond: Rep[Boolean], msg: String): Unit = if (cond) { printf(msg); exit(0) }

      @virtualize
      def snippet(arg: Rep[Int]) = {
        myassert(arg > 0, "fail")
      }
    }
    check("exit2", driver.code, "c")
  }
}
