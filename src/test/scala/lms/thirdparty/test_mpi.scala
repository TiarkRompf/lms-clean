package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
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
      @virtualize
      def snippet(arg: Rep[Int]) = {
        mutate(arg)
        printf("%d", arg)
      }
    }
    System.out.println(indent(driver.code))
  }

  test("mpi-hello-world") {
    val driver = new DslDriverCMPI[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        mpi_init()

        var world_size = 0
        mpi_comm_size(mpi_comm_world, Pointer(world_size))

        var world_rank = 1
        mpi_comm_rank(mpi_comm_world, Pointer(world_rank))

        val processor_name = NewArray[Char](mpi_max_processor_name)
        var name_len = 0
        mpi_get_processor_name(processor_name, Pointer(name_len))

        printf("Hellw world from processor %s, rank %d out of %d processors\n",
          processor_name, world_rank, world_size)
        mpi_finalize()
      }
    }
    System.out.println(indent(driver.code))
  }
}
