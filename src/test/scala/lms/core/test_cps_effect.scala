package lms
package core

import stub._
import macros.SourceContext
import utils.time

import java.io.File
import java.io.PrintWriter

import scala.util.continuations._
import scala.util.continuations


class CPSEffectTest extends TutorialFunSuite {
  val under = "cps_effect/"

  test("interactWithCTRL") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          println("A")
          shift1[Int, Int] { k => k(k(arg)) }
          println("B")
          1
        } + 5
      }
    }
    assert(driver.eval(0) == 6)
    assert(driver.eval2(0) == 6)
    assert(driver.eval3(0) == 6)
    // test source
    val src = driver.code
    checkOut("interactWithCTRL", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("interactWithCTRLTrans", "scala", {
      println(src2)
      println("// output:")
    })
    val src3 = driver.code3
    checkOut("interactWithCTRLTransSelective", "scala", {
      println(src3)
      println("// output:")
    })
  }

  test("interactWithCTRL1") {
    val driver = new CPSDslDriver[Int, Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        reset1 {
          println("A")
          println("B")
          shift1[Int, Int] { k => k(k(arg)) }
          1
        } + 5
      }
    }
    driver.eval(0)
    driver.eval2(0)
    driver.eval3(0)
    // test source
    val src = driver.code
    checkOut("interactWithCTRL1", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("interactWithCTRL1Trans", "scala", {
      println(src2)
      println("// output:")
    })
    val src3 = driver.code3
    checkOut("interactWithCTRL1TransSelective", "scala", {
      println(src3)
      println("// output:")
    })
  }

  test("interactWithWrite0") {
    val driver = new CPSDslDriver[Int,Int] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Int] = {
        var a = 3
        var b = 2 // Node: ideally b should be removed, but I need to do DCE for it.
        reset1 {
          a += 2
          b += 3
        }
        a
      }
    }
    assert(driver.eval(0) == 5)
    assert(driver.eval2(0) == 5)
    assert(driver.eval3(0) == 5)
    // test source
    val src = driver.code
    checkOut("interactWithWrite0", "scala", {
      println(src)
      println("// output:")
    })
    val src2 = driver.code2
    checkOut("interactWithWrite0Trans", "scala", {
      println(src2)
      println("// output:")
    })
    val src3 = driver.code3
    checkOut("interactWithWrite0TransSelective", "scala", {
      println(src3)
      println("// output:")
    })
  }

  /**

  This test reveals several problems with the current effect system.
  1. if var a = 3 is in reset, it is a local variable to reset, and it will be removed
     Partial solution1: for reset block, do not remove local variables from dependency (patch fix, not good)
  2. application of k in shift is common subexpression eliminated. 
     Partial solution1: for getLatentEffect in backend.scala, for the Case None => give CTRL effect (patch fix, break other tests)
  Ideal solution:

     Everything after first application of k in shift should contribute to the effect of continuation.
  **/
  test("interactWithWrite-1") {
    val driver = new CPSDslDriver[Int, Unit] {

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        var a = 3
        reset1 {
          shift1[Int, Unit] { k =>
            println(a)
            k(1); k(1)
            println(a)
          }
          a += 2
        }
      }
    }
    // driver.eval(0)
    // driver.eval2(0)
    // driver.eval3(0)
    // test source
    // val src = driver.code
    // checkOut("interactWithWrite-1", "scala", {
    //   println(src)
    //   println("// output:")
    // })
    // val src2 = driver.code2
    // checkOut("interactWithWrite-1Trans", "scala", {
    //   println(src2)
    //   println("// output:")
    // })
    // val src3 = driver.code3
    // checkOut("interactWithWrite-1TransSelective", "scala", {
    //   println(src3)
    //   println("// output:")
    // })
  }

  // test("interactWithWrite") {
  //   val driver = new CPSDslDriver[Int, Unit] {

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       var a = 3
  //       var b = 2
  //       reset1 {
  //         println("A")
  //         shift1[Int, Unit] { k =>
  //           k(1); k(1)
  //         }
  //         a += 2
  //         b += 3
  //         println(a)
  //       }
  //     }
  //   }
  //   driver.eval(0)
  //   driver.eval2(0)
  //   driver.eval3(0)
  //   // test source
  //   val src = driver.code
  //   checkOut("interactWithWrite", "scala", {
  //     println(src)
  //     println("// output:")
  //   })
  //   val src2 = driver.code2
  //   checkOut("interactWithWriteTrans", "scala", {
  //     println(src2)
  //     println("// output:")
  //   })
  //   val src3 = driver.code3
  //   checkOut("interactWithWriteTransSelective", "scala", {
  //     println(src3)
  //     println("// output:")
  //   })
  // }

  // test("interactWithWrite1") {
  //   val driver = new CPSDslDriver[Int, Unit] {

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       var a = 3
  //       var b = 5
  //       def f() = shift1[Int, Unit] { k => k(1); k(1) }
  //       reset1 {
  //         println("A")
  //         f()
  //         a += 2
  //         println(a)
  //       }
  //       reset1 {
  //         println("B")
  //         f()
  //         b += 5
  //         println(b)
  //       }
  //     }
  //   }
  //   driver.eval(0)
  //   driver.eval2(0)
  //   driver.eval3(0)
  //   // test source
  //   val src = driver.code
  //   checkOut("interactWithWrite1", "scala", {
  //     println(src)
  //     println("// output:")
  //   })
  //   val src2 = driver.code2
  //   checkOut("interactWithWrite1Trans", "scala", {
  //     println(src2)
  //     println("// output:")
  //   })
  //   val src3 = driver.code3
  //   checkOut("interactWithWrite1TransSelective", "scala", {
  //     println(src3)
  //     println("// output:")
  //   })
  // }

  // test("interactWithWrite2") {
  //   val driver = new CPSDslDriver[Int, Unit] {

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       var a = 3
  //       var b = 5
  //       def f() = shift1[Int, Unit] { k => k(1); k(1) }
  //       reset1 {
  //         println("A")
  //         f()
  //         a += 2
  //         b += 5
  //         println(a)
  //       }
  //       reset1 {
  //         println("B")
  //         f()
  //         a += 2
  //         b += 5
  //         println(b)
  //       }
  //     }
  //   }
  //   driver.eval(0)
  //   driver.eval2(0)
  //   driver.eval3(0)
  //   // test source
  //   val src = driver.code
  //   checkOut("interactWithWrite2", "scala", {
  //     println(src)
  //     println("// output:")
  //   })
  //   val src2 = driver.code2
  //   checkOut("interactWithWrite2Trans", "scala", {
  //     println(src2)
  //     println("// output:")
  //   })
  //   val src3 = driver.code3
  //   checkOut("interactWithWrite2TransSelective", "scala", {
  //     println(src3)
  //     println("// output:")
  //   })
  // }
}
