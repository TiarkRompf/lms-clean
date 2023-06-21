package lms
package core

import core._
import core.stub._
import macros.SourceContext
import macros.RefinedManifest

import lms.collection.mutable._

class StructTest extends TutorialFunSuite {
  val under = "backend/"

  test("basic_struct_is_OK") {
    val driver = new DslDriverC[Int, Double] with StructOps { q =>
      override val codegen = new DslGenC with CCodeGenStruct {
        val x = 1
        val IR: q.type = q
      }

      @CStruct case class Complex(real: Double, image: Double)

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val s = Pointer.local[Complex]
        s.real = 1.23 // that is s.writeField("real", 1.23)
        s.image = 2.34
        val s2 = Pointer(s.deref)
        s2.real = 5.0
        val s3 = Pointer(s2.deref)
        s3.real = 10.0
        s3.real + s.image // that is s.readField("real") ...
      }
    }
    check("basic_struct", driver.code, "c")
  }
}
