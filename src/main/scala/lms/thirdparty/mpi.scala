package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

trait MPIOps extends lms.collection.PointerOps { b: Base =>
  /* LMS support for MPI library */

  def mpi_init(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectWrite("mpi-init")(Adapter.CTRL))

  abstract class MPIWorld extends Manifest[MPIWorld]
  lazy val mpi_comm_world: Rep[MPIWorld] = Wrap[MPIWorld](Adapter.g.reflect("mpi-comm-world"))

  def mpi_comm_size(world: Rep[MPIWorld], size: Rep[Pointer[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-comm-size", Unwrap(world), Unwrap(size))(Unwrap(Pointer.deref(size))))
  }

  def mpi_comm_rank(world: Rep[MPIWorld], rank: Rep[Pointer[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-comm-rank", Unwrap(world), Unwrap(rank))(Unwrap(Pointer.deref(rank))))
  }

  lazy val mpi_max_processor_name: Rep[Int] = Wrap[Int](Adapter.g.reflect("mpi-max-processor-name"))

  def mpi_get_processor_name(name: Rep[Array[Char]], len: Rep[Pointer[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-get-processor-name", Unwrap(name), Unwrap(len))
    (Unwrap(name), Unwrap(Pointer.deref(len))))
  }

  def mpi_finalize(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectWrite("mpi-finalize")(Adapter.CTRL))

  // this mutate method is a made up method
  def mutate(x: Rep[Int]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("macro-mutate", Unwrap(x))(Unwrap(x)))
  }

  object DataStructure1 {
    def apply()(implicit pos: SourceContext): Rep[DataStructure1] = {
      Wrap[DataStructure1](Adapter.g.reflect("datastructure1-new"))
    }
  }
  abstract class DataStructure1 extends Manifest[DataStructure1]
}

trait CCodeGenMPI extends ExtendedCCodeGen with lms.collection.CCodeGenPointer {
  override def remap(m: Manifest[_]): String = {
    System.out.println(m.runtimeClass.getName)
    if (m.runtimeClass.getName == "lms.thirdparty.MPIOps$MPIWorld") {
      "auto" // can this be delt with in a better way?
    } else { super.remap(m) }
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "macro-mutate", List(x), _) =>
      emit("MUTATE("); shallow(x); emit(")")
    case Node(s, "mpi-comm-world", _, _) =>
      emit("MPI_COMM_WORLD")
    case Node(s, "mpi-init", _, _) =>
      emit("MPI_INIT(NULL, NULL)")
    case Node(s, "mpi-comm-size", List(a, b), _) =>
      emit("MPI_COMM_SIZE("); shallow(a); emit(", "); shallow(b); emit(")")
    case Node(s, "mpi-comm-rank", List(a, b), _) =>
      emit("MPI_COMM_RANK("); shallow(a); emit(", "); shallow(b); emit(")")
    case Node(s, "mpi-max-processor-name", _, _) =>
      emit("MPI_MAX_PROCESSOR_NAME")
    case Node(s, "mpi-get-processor-name", List(a, b), _) =>
      emit("MPI_Get_processor_name("); shallow(a); emit(", "); shallow(b); emit(")")
    case Node(s, "mpi-finalize", _, _) =>
      emit("MPI_FINALIZE()")
    case _ => super.shallow(n)
  }
}
