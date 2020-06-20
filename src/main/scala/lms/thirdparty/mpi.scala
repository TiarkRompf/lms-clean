package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._

trait MPIOps extends CMacro with LibStruct { b: Base =>
  /* LMS support for MPI library */

  // this is how we deal with constant macros
  class MPIWorld
  def mpi_comm_world: Rep[MPIWorld] = cmacro[MPIWorld]("MPI_COMM_WORLD")
  def mpi_max_processor_name: Rep[Int] = cmacro[Int]("MPI_MAX_PROCESSOR_NAME")

  // this is how we deal with library functions (may need pointers)
  def mpi_init(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectWrite("mpi-init")(Adapter.CTRL))
  def mpi_finalize(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectWrite("mpi-finalize")(Adapter.CTRL))

  def mpi_comm_size(world: Rep[MPIWorld], size: Var[Int]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-comm-size", Unwrap(world), UnwrapV(size))(UnwrapV(size)))
  }

  def mpi_comm_rank(world: Rep[MPIWorld], rank: Var[Int]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-comm-rank", Unwrap(world), UnwrapV(rank))(UnwrapV(rank)))
  }

  def mpi_get_processor_name(name: Rep[Array[Char]], len: Var[Int]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("mpi-get-processor-name", Unwrap(name), UnwrapV(len))
    (Unwrap(name), UnwrapV(len)))
  }

  // this is how we bind to library structs
  abstract class DataStructure1
  def dataStructure1 = libStruct[DataStructure1]("DataStructure1")

  // some example (dummy code). Please remove later :)
  def test_pointer(x: Var[Int]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("test-pointer", UnwrapV(x))(Adapter.CTRL, UnwrapV(x)))
  }
  def test_pointer(x: Rep[Array[Int]]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("test-pointer-array", Unwrap(x))(Adapter.CTRL, Unwrap(x)))
  }
  def test_struct_pointer(x: Rep[DataStructure1]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("test-struct-pointer", Unwrap(x))(Adapter.CTRL, Unwrap(x)))
  }
  def test_struct_reference(x: Rep[DataStructure1]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("test-struct-ref", Unwrap(x))(Adapter.CTRL, Unwrap(x)))
  }
}

trait CCodeGenMPI extends ExtendedCCodeGen {
  override def remap(m: Manifest[_]): String =
    m.runtimeClass.getName match {
      case "lms.thirdparty.MPIOps$DataStructure1" => "DataStructure1"
      case _ => super.remap(m)
    }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "mpi-init", _, _) =>
      emit("MPI_INIT(NULL, NULL)")
    case Node(s, "mpi-comm-size", List(a, b), _) =>
      emit("MPI_COMM_SIZE("); shallow(a); emit(", &"); shallow(b); emit(")")
    case Node(s, "mpi-comm-rank", List(a, b), _) =>
      emit("MPI_COMM_RANK("); shallow(a); emit(", &"); shallow(b); emit(")")
    case Node(s, "mpi-get-processor-name", List(a, b), _) =>
      emit("MPI_Get_processor_name("); shallow(a); emit(", &"); shallow(b); emit(")")
    case Node(s, "mpi-finalize", _, _) =>
      emit("MPI_FINALIZE()")

    case Node(s, "test-pointer", List(x:Sym), _) =>
      emit("test_pointer(&"); shallow(x); emit(")");
    case Node(s, "test-pointer-array", List(x:Sym), _) =>
      emit("test_pointer("); shallow(x); emit(")");
    case Node(s, "test-struct-pointer", List(x:Sym), _) =>
      emit("test_struct_pointer(&"); shallow(x); emit(")")
    case Node(s, "test-struct-ref", List(x:Sym), _) =>
      emit("test_struct_pointer("); shallow(x); emit(")")
    case _ => super.shallow(n)
  }

}
