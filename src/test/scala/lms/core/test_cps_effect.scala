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
    driver.eval(0)
    driver.eval2(0)
    driver.eval3(0)
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


}
