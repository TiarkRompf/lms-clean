package lms
package thirdparty

import lms.core.stub._
import macros.SourceContext

class MPITest extends TutorialFunSuite {
  val under = "thirdparty/mpi/"

  abstract class DslDriverCMPI[A:Manifest, B:Manifest] extends DslDriverC[A,B]
    with lms.thirdparty.MPIOps { q =>
      override val codegen = new DslGenC with lms.thirdparty.CCodeGenMPI {
        val IR: q.type = q
      }
  }

  test("mpi-macro-1") {
    val driver = new DslDriverCMPI[Int,Unit] {
      def snippet(arg: Rep[Int]) = {
        mutate(arg)
        printf("%d", arg)
      }
    }
    System.out.println(indent(driver.code))
  }

}
