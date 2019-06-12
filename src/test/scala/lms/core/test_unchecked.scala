package lms
package core

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext

class UncheckedTest extends TutorialFunSuite {
  val under = "core/"

  test("uncheckedPure_write") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        val y: Rep[Int] = x
        x = 5
        uncheckedPureWrite[Unit]("foo(&",x,")")(x)
        printf("%d %d\n", y, x)
      }
    }
    check("uncheckedPure_write", driver.code, "c")
  }
  test("uncheckedPure_write_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        val y: Rep[Int] = x
        uncheckedPureWrite[Unit]("foo(&",x,")")(x)
        printf("%d\n", y)
      }
    }
    check("uncheckedPure_write_dead", driver.code, "c")
  }
  test("unchecked_write") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        val y: Rep[Int] = x
        x = 5
        uncheckedWrite[Unit]("foo(&",x,")")(x)
        printf("%d %d\n", y, x)
      }
    }
    check("unchecked_write", driver.code, "c")
  }
  test("unchecked_write_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        val y: Rep[Int] = x
        x = 5
        uncheckedWrite[Unit]("foo(&",x,")")(x)
        printf("%d\n", y)
      }
    }
    check("unchecked_write_dead", driver.code, "c")
  }
  test("unchecked_read") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        val y = uncheckedRead[Int]("foo(",x,")")(x)
        printf("%d\n", y)
      }
    }
    check("unchecked_read", driver.code, "c")
  }
  test("unchecked_read_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        x = 5
        val y = uncheckedRead[Int]("foo(",x,")")(x)
        printf("Hello\n")
      }
    }
    check("unchecked_read_dead", driver.code, "c")
  }
  test("unchecked_vars_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        var y = arg + 5
        val xv: Rep[Int] = x
        val yv: Rep[Int] = y
        val z = unchecked[Int]("foo(&",x,", ",y,")")
        printf("%d %d\n", xv, yv)
      }
    }
    check("unchecked_vars_dead", driver.code, "c")
  }
  test("uncheckedPure_vars_dead") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = arg
        var y = arg + 5
        val xv: Rep[Int] = x
        val yv: Rep[Int] = y
        val z = uncheckedPure[Int]("foo(",x,", ",y,")")
        printf("%d\n", xv)
        printf("%d\n", yv)
      }
    }
    check("uncheckedPure_vars_dead", driver.code, "c")
  }
}
