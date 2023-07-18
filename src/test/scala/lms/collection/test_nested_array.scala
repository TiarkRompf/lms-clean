package lms
package core

import core._
import core.stub._
import macros.SourceContext
import macros.RefinedManifest

import lms.collection.mutable._

class NestedArrayTest extends TutorialFunSuite {
  val under = "backend/"

  test("nested_array_is_ok") {
    val driver = new DslDriverC[Array[Array[Int]], Array[Array[Int]]] with StructOps { q =>
      override val codegen = new DslGenC with CCodeGenStruct {
        val IR: q.type = q
      }

      @virtualize
      def snippet(arg: Rep[Array[Array[Int]]]) = {
        arg(0)(0) = 3
        arg
      }
    }
    check("nested_array", driver.code, "c")
  }
}
