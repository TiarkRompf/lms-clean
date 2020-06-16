package lms
package collection

import lms.core.stub._
import lms.core.virtualize
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
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var v = 0
        val a = Pointer(v)
        printf("address is %p", a)
      }
    }
    System.out.println(indent(driver.code))
  }

  test("pointer-2") {
    val driver = new DslDriverCPointer[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var v = 0
        val f = topFun { (a: Rep[Pointer[Int]]) =>
          printf("A library function that asks for pointers as parameter")
        }
        f(Pointer(v))
      }
    }
    System.out.println(indent(driver.code))
  }
}
