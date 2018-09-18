/**
Generating C directly
=====================

*/

package lms
package experimental

import scala.annotation.implicitNotFound

class CGenTest extends TutorialFunSuite {
  val under = "experimental/cgen-"

  test("01") {
    val res = utils.captureOut {
      import c._

      gen"""
      void main(argc: int, argv: char**) {
        puts("Hello!");
      }
      """
    }
    check("01", res)
  }

  test("02") {
    val res = utils.captureOut {
      import c._

      case class StringE(exp: Exp)
      implicit def lift(s: String) = StringE(Exp("\""+s+"\""))

      def puts(s: StringE) = gen"""
        puts(${s.exp});
      """

      def body() = puts("Hello!")

      val argc, argv = fresh
      gen"""
      void main($argc: int, $argv: char**) {
        ${body()}
      }
      """
    }
    check("02", res)
  }

}