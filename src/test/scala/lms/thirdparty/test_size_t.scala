package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import macros.SourceContext
import lms.collection._

class SizeTTest extends TutorialFunSuite {
  val under = "thirdparty/size_t/"

  abstract class DslDriverCSizeT[A: Manifest, B: Manifest] extends DslDriverC[A,B] with SizeTOps { q =>
    override val codegen = new DslGenC with CCodeGenSizeTOps {
      val IR: q.type = q
    }
  }

  test("basic") {
    val driver = new DslDriverCSizeT[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = topFun { a: Rep[SizeT] =>
          printf("size t is %d", a.toInt)
        }
        f(SizeT(5))
        f(SizeT(arg * 6))
      }
    }
    check("size_t", driver.code, "c")
    driver.eval(0)
  }

  test("size_of") {
    val driver = new DslDriverCSizeT[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val arr = malloc[Int](SizeT(10))
        val arr2 = mallocOfT[Int](10)
        memset(arr, 0, SizeT(10))
        memcpy(arr2, arr, SizeT(10))
        printf("%d %d", arr(1), arr2(1))
      }
    }
    check("size_of", driver.code, "c")
    driver.eval(9)
  }
}
