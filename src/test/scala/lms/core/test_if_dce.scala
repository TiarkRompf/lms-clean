package lms
package core

import lms.core.stub._
import lms.macros.SourceContext

class IfDCETest extends TutorialFunSuite {
  val under = "core/"

  test("if_dead_unchecked") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        if (arg == 0) { uncheckedPure[Int]("foo") } else  { unchecked[Int]("bar") }
        printf("hello")
      }
    }
    check("if_dead_unchecked", driver.code, "c")
  }

  test("if_dead_pure") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        if (arg == 0) { 1 + arg } else  { 3 }
        printf("hello")
      }
    }
    check("if_dead_pure", driver.code, "c")
  }
  test("if_dead_effect") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        if (arg == 0) { printf("hkjfd\n") } else  { printf("fsgl") }
      }
    }
    check("if_dead_effect", driver.code, "c")
  }
  test("if_dead_effect_value") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        if (arg == 0) { printf("hkjfd\n"); 3 } else  { printf("fsgl"); arg + 6 }
        printf("hello")
      }
    }
    check("if_dead_effect_value", driver.code, "c")
  }
  test("if_unchecked") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = if (arg == 0) { uncheckedPure[Int]("foo") } else  { unchecked[Int]("bar") }
        printf(">> %d\n", x)
      }
    }
    check("if_unchecked", driver.code, "c")
  }
  test("if_pure") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = if (arg == 0) { 1 + arg } else  { 3 }
        printf(">> %d\n", x)
      }
    }
    check("if_pure", driver.code, "c")
  }
  test("if_effect") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = if (arg == 0) { printf("hkjfd\n"); 3 } else  { printf("fsgl"); arg + 6 }
        printf(">> %d\n", x)
      }
    }
    check("if_effect", driver.code, "c")
  }

  test("if_effect_reifyHere") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 0
        var y = 0
        if (arg == 0) {
          x += 1
          y += 5
        } else {
          x += 3
        }
        printf("%d\n", y)
      }
    }
    check("if_effect_reifyHere", driver.code, "c")
  }

}
