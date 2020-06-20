package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import macros.SourceContext
import lms.collection._

class MPITest extends TutorialFunSuite {
  val under = "thirdparty/mpi/"

  abstract class DslDriverCMPI[A:Manifest, B:Manifest] extends DslDriverC[A,B] with MPIOps { q =>
    override val codegen = new DslGenC with CCodeGenMPI with CCodeGenCMacro {
      val IR: q.type = q
    }
  }

  test("pointer-var") {
    val driver = new DslDriverCMPI[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        var v = 0
        test_pointer(v) // The idea is: if a fun want pointer, we say it needs Var[Int] as input type
        // then we generate & for it :) So Var[T] is use to mark if we need &, not if it is mutated or not
        val arr = NewArray[Int](10)
        test_pointer(arr);

        val obj = DataStructure1()
        test_struct_pointer(obj);
        test_struct_reference(obj)
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
        mpi_comm_size(mpi_comm_world, world_size)

        var world_rank = 0
        mpi_comm_rank(mpi_comm_world, world_rank)

        val processor_name = NewArray[Char](mpi_max_processor_name)
        var name_len = 0
        mpi_get_processor_name(processor_name, name_len)

        printf("Hellw world from processor %s, rank %d out of %d processors\n",
          processor_name.ArrayOfCharToString, world_rank, world_size)
        mpi_finalize()
      }
    }
    System.out.println(indent(driver.code))
  }

  test("mpi-hello-world2") {
    val driver = new DslDriverCMPI[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        mpi_init()

        val f = fun { x: Rep[Int] =>
          var world_size = 0
          mpi_comm_size(mpi_comm_world, world_size)

          var world_rank = 0
          mpi_comm_rank(mpi_comm_world, world_rank)

          printf("%d, %d", world_size, world_rank)
        }

        f(arg)

        mpi_finalize()
      }
    }
    System.out.println(indent(driver.code))
  }

  test("mpi-data-structure") {
    val driver = new DslDriverCMPI[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val a = DataStructure1()
        val b = NewArray[Int](10)
        val f = topFun { (a: Rep[DataStructure1]) =>
          printf("a library function that asks for pointers as parameter")
        }
        f(a)
      }
    }
    System.out.println(indent(driver.code))
  }

  test("mpi-macro-const-2") {
    val driver = new DslDriverCMPI[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val f = topFun { (a: Rep[Int]) => a + 1 }
        val a = f(mpi_max_processor_name)
        val b = f(mpi_max_processor_name + 1)
        printf("%d %d\n", a, b)
      }
    }
    System.out.println(indent(driver.code))
  }
}
