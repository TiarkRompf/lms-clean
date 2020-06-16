package lms
package core

import stub._
import macros.SourceContext

class PointerTest extends TutorialFunSuite {
  val under = "experimental/pointer/"

  abstract class DslDriverCPointer[A:Manifest, B:Manifest] extends DslDriverC[A,B] { q =>
    override val codegen = new DslGenC with lms.collection.CCodeGenPointer {
      val IR: q.type = q
    }
  }

  test("pointer-1") {
    val driver = new DslDriverCPointer[Int,Unit] with lms.collection.PointerOps {
      def snippet(arg: Rep[Int]) = {
        val a = Pointer(arg)
        printf("address is %p", a)
      }
    }
    System.out.println(indent(driver.code))
  }
}
