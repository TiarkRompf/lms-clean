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
        val IR: q.type = q
      }

      abstract class Complex extends Struct

      implicit val complex = new RefinedManifest[Complex] {
        def fields: List[(String, Manifest[_])] = List(("real", manifest[Double]), ("image", manifest[Double]))
        def runtimeClass = classOf[Complex]
      }

      @virtualize
      def snippet(arg: Rep[Int]) = {
        val s = Pointer.local[Complex]
        s.writeField("real", 1.23)
        s.writeField("image", 2.34)
        s.readField[Double]("real") + s.readField[Double]("image")
      }
    }
    check("basic_struct", driver.code, "c")
  }
}
