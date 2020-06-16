package lms
package collection

import lms.core.stub._
import macros.SourceContext

class PointerTest extends TutorialFunSuite {
  val under = "experimental/pointer/"

  abstract class DslDriverCPointer[A:Manifest, B:Manifest] extends DslDriverC[A,B]
    with lms.collection.PointerOps { q =>
      override val codegen = new DslGenC with lms.collection.CCodeGenPointer {
        val IR: q.type = q
      }
  }

  test("pointer-1") {
    val driver = new DslDriverCPointer[Int,Unit] {
      def snippet(arg: Rep[Int]) = {
        val a = Pointer(arg)
        printf("address is %p", a)
      }
    }
    System.out.println(indent(driver.code))
  }

  test("pointer-2") {
    val driver = new DslDriverCPointer[Int,Unit] {
      def snippet(arg: Rep[Int]) = {
        val f = topFun { (a: Rep[Pointer[Int]]) =>
          printf("A library function that asks for pointers as parameter")
        }
        f(Pointer(arg))
      }
    }
    System.out.println(indent(driver.code))
  }
}
