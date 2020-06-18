package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._
trait MPIOps { b: Base with PointerOps =>
  /* LMS support for MPI library */

  def mpi_init(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectWrite("mpi-init")(Adapter.CTRL))

  class MPIWorld
  object MPICommWorld extends MPIWorld {
    override def toString = "MPI_COMM_WORLD"
  }
  lazy val mpi_comm_world: Rep[MPIWorld] = unit(MPICommWorld)

  def mpi_comm_size(world: Rep[MPIWorld], size: Rep[Pointer[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-comm-size", Unwrap(world), Unwrap(size))(Unwrap(Pointer.deref(size))))
  }

  def mpi_comm_rank(world: Rep[MPIWorld], rank: Rep[Pointer[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-comm-rank", Unwrap(world), Unwrap(rank))(Unwrap(Pointer.deref(rank))))
  }

  lazy val mpi_max_processor_name: Rep[Int] = Wrap[Int](Adapter.g.reflect("INLINE_mpi-max-processor-name"))

  def mpi_get_processor_name(name: Rep[Array[Char]], len: Rep[Pointer[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-get-processor-name", Unwrap(name), Unwrap(len))
    (Unwrap(name), Unwrap(Pointer.deref(len))))
  }

  def mpi_finalize(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectWrite("mpi-finalize")(Adapter.CTRL))

  object DataStructure1 {
    def apply()(implicit pos: SourceContext): Rep[DataStructure1] = {
      Wrap[DataStructure1](Adapter.g.reflect("local_struct"))
    }
  }
  abstract class DataStructure1 extends CStruct
}

trait CCodeGenMPI extends ExtendedCCodeGen {
  override def remap(m: Manifest[_]): String =
    m.runtimeClass.getName match {
      case "lms.thirdparty.MPIOps$MPIWorld" => "MPIWorld"
      case "lms.thirdparty.MPIOps$DataStructure1" => "DataStructure1"
      case _ => super.remap(m)
    }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "macro-mutate", List(x), _) =>
      emit("MUTATE("); shallow(x); emit(")")
    case Node(s, "mpi-init", _, _) =>
      emit("MPI_INIT(NULL, NULL)")
    case Node(s, "mpi-comm-size", List(a, b), _) =>
      emit("MPI_COMM_SIZE("); shallow(a); emit(", "); shallow(b); emit(")")
    case Node(s, "mpi-comm-rank", List(a, b), _) =>
      emit("MPI_COMM_RANK("); shallow(a); emit(", "); shallow(b); emit(")")
    case Node(s, "INLINE_mpi-max-processor-name", _, _) =>
      emit("MPI_MAX_PROCESSOR_NAME")
    case Node(s, "mpi-get-processor-name", List(a, b), _) =>
      emit("MPI_Get_processor_name("); shallow(a); emit(", "); shallow(b); emit(")")
    case Node(s, "mpi-finalize", _, _) =>
      emit("MPI_FINALIZE()")
    case _ => super.shallow(n)
  }

}
