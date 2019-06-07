package lms
package core

import stub._
import macros.SourceContext

class SwitchTest extends TutorialFunSuite {
  val under = "core/"

  test("switch-1c") {
    val driver = new DslDriverC[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        var y = 0
        switch(arg, Some(() => printf("%d\n", arg)))(
          (Seq(0),
            x => printf("zero %d\n", x * 8)),
          (Seq(1, 2, 3),
            x => y = x)
        )
        printf("Final value %d\n", y)
      }
    }
    val src = driver.code
    checkOut("switch_1", "c", {
      println(src)
      println("// Output:")
      driver.eval(0)
      driver.eval(2)
      driver.eval(5)
    })
  }

  test("switch-1scala") {
    val driver = new DslDriver[Int,Unit] {

      @virtualize
      def snippet(arg: Rep[Int]) = {
        var y = 0
        switch(arg, Some(() => printf("%d\n", arg)))(
          (Seq(0),
            x => printf("zero %d\n", x * 8)),
          (Seq(1, 2, 3),
            x => y = x)
        )
        printf("Final value %d\n", y)
      }
    }
    val src = driver.code
    checkOut("switch_1", "scala", {
      println(src)
      println("// Output:")
      utils.devnull(driver.precompile)
      driver.eval(0)
      driver.eval(2)
      driver.eval(5)
    })
  }

  test("switch-2scala") {
    val driver = new DslDriver[String,Unit] {

      @virtualize
      def snippet(arg: Rep[String]) = {
        switch(arg) (
          (Seq("hello"),
            x => println(x)),
          (Seq("A", "B", "C"),
            { x => println("Hello"); println(x) })
        )
      }
    }
    val src = driver.code
    checkOut("switch_2", "scala", {
      println(src)
      println("// Output:")
      driver.eval("A")
      driver.eval("C")
      driver.eval("hello")
    })
  }
}
