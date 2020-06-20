package lms
package core

import stub._
import macros.SourceContext

class ArrayTest extends TutorialFunSuite {
  val under = "backend/"

  test("free-1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        printf("%d\n", x(0))
        x.free
      }
    }
    check("free_1", driver.code, "c")
  }

  test("free-2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        var idx = 0
        while (idx < 10) {
          x(idx) = idx
          idx += 1
        }
        printf(">> %d\n", x(5))
        x.free
      }
    }
    check("free_2", driver.code, "c")
  }

  test("free-dead1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        x.free
      }
    }
    check("free_dead1", driver.code, "c")
  }

  test("free-dead2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        x(0) = 1
        x.free
      }
    }
    check("free_dead2", driver.code, "c")
  }

  test("free-dead3") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        val y = NewArray[Int](10)
        printf("%d\n", x(0))
        y.free
        x.free
      }
    }
    check("free_dead3", driver.code, "c")
  }

  test("free-dead4") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        var idx = 0
        while (idx < 10) {
          x(idx) = idx
          idx += 1
        }
        x.free
      }
    }
    check("free_dead4", driver.code, "c")
  }

  test("array-copy-1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        val y = NewArray[Int](10)
        var idx = 0
        while (idx < 10) {
          x(idx) = idx
          idx += 1
        }
        x.copyToArray(y, 0, 10)
        idx = 0
        while (idx < 10) {
          printf("%d\n", y(idx))
          idx += 1
        }
      }
    }
    check("array_copy_1", driver.code, "c")
  }

  test("array-copy-2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Char](10)
        val y = unit("Hello")
        uncheckedPure[Array[Char]](y).copyToArray(x, 0, 5)
        printf("%5s\n", uncheckedPureRead[String](x)(x))
      }
    }
    check("array_copy_2", driver.code, "c")
  }

  test("realloc-1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewLongArray[Int](10L)
        printf("%d\n", x(0L))
        val y = x.resize(20L)
        printf("%d\n", y(5L))
        y.free
      }
    }
    check("realloc_1", driver.code, "c")
  }
  test("realloc-2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewLongArray[Int](10L)
        printf("%d\n", x(0L))
        val y = x.resize(20L)
        y.free
      }
    }
    check("realloc_2", driver.code, "c")
  }
  test("realloc-3") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewLongArray[Int](10L)
        x(0L) = 3
        val y = x.resize(20L)
        y(4L) = 3
        printf("%d\n", y(3L))
        y.free
      }
    }
    check("realloc_3", driver.code, "c")
  }
  test("realloc-4") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewLongArray[Int](10L)
        x(0L) = 3
        val y = x.resize(20L)
        printf("%d\n", y(3L))
        y.free
      }
    }
    check("realloc_4", driver.code, "c")
  }
  test("realloc-dead1") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewLongArray[Int](10L)
        val y = x.resize(20L)
        y.free
      }
    }
    check("realloc_dead1", driver.code, "c")
  }
  test("realloc-dead2") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewLongArray[Int](10L)
        x(0L) = 3
        val y = x.resize(20L)
        y(4L) = 3
        y.free
      }
    }
    check("realloc_dead2", driver.code, "c")
  }
  test("array_opt_set_equal") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = NewArray[Int](10)
        x(0) = arg
        x(0) = arg
        printf("%d", x(1))
      }
    }
    check("array_opt_set_equal", driver.code, "c")
  }

  test("extraThroughSoft_is_necessary") {
    val driver = new DslDriverC[Int,Unit] {
      @virtualize
      def snippet(in: Rep[Int]) = {
        val x = NewArray[Int](in + 3)
        val y = x(in)
        x(in) = 5
        if (in == 0) printf("%d\n", y)
        printf("%d", x(in + 2))
      }
    }
    check("extraThroughSoft_is_necessary", driver.code, "c")
  }
}
