package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import macros.SourceContext
import lms.collection._

class RandomDataTest extends TutorialFunSuite {
  val under = "thirdparty/random_data/"

  abstract class DslDriverCRandomData[A: Manifest, B: Manifest] extends DslDriverC[A,B] with RandomDataOps { q =>
    override val codegen = new DslGenC with CCodeGenLibs {
      val IR: q.type = q
    }
  }

  test("int") {
    val driver = new DslDriverCRandomData[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        for (i <- (0 until 10): Rep[Range]) {
          printf("%d ", randInt + arg)
        }
      }
    }
    check("rand_int", driver.code, "c")
    driver.eval(0)
  }

  test("float") {
    val driver = new DslDriverCRandomData[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        for (i <- (0 until 10): Rep[Range]) {
          printf("%f ", randFloat)
        }
      }
    }
    check("rand_float", driver.code, "c")
    driver.eval(9)
  }
}
