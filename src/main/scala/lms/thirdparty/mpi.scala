package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._

trait MPIOps extends CMacro with LibStruct with LibFunction { b: Base =>
  /* LMS support for MPI library */

  // this is how we deal with constant macros
  class MPIWorld
  def mpi_comm_world: Rep[MPIWorld] = cmacro[MPIWorld]("MPI_COMM_WORLD")
  def mpi_max_processor_name: Rep[Int] = cmacro[Int]("MPI_MAX_PROCESSOR_NAME")

  class CNull
  def cNull: Rep[CNull] = cmacro[CNull]("NULL")

  // this is how we deal with library functions (may need pointers)
  def mpi_init() = libFunction[Unit]("MPI_INIT", Unwrap(cNull), Unwrap(cNull))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)
  def mpi_finalize() = libFunction[Unit]("MPI_FINALIZE")(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  def mpi_comm_size(world: Rep[MPIWorld], size: Var[Int]): Rep[Unit] = {
    libFunction[Unit]("MPI_COMM_SIZE", Unwrap(world), UnwrapV(size))(Seq[Int](), Seq(1), Set(1))
  }
  def mpi_comm_rank(world: Rep[MPIWorld], rank: Var[Int]): Rep[Unit] = {
    libFunction[Unit]("MPI_COMM_RANK", Unwrap(world), UnwrapV(rank))(Seq[Int](), Seq(1), Set(1))
  }

  def mpi_get_processor_name(name: Rep[Array[Char]], len: Var[Int]): Rep[Unit] = {
    libFunction[Unit]("MPI_Get_processor_name", Unwrap(name), UnwrapV(len))(Seq[Int](), Seq(0, 1), Set(1))
  }

  // this is how we bind to library structs with field read access
  abstract class DataStructure1
  implicit class DataStructure1Ops(x: Rep[DataStructure1]) {
    val fieldA: Rep[Int] = readField[DataStructure1, Int](x, "fieldA")
  }
  def dataStructure1 = libStruct[DataStructure1]("DataStructure1")

  // some example (dummy code). this is using the old way of reflecting new nodes, which requires changes in code gen :)
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
