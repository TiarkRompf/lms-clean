package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._

trait MPIOps extends CMacro with LibStruct with LibFunction {
  /* LMS support for MPI library */

  // this is how we deal with constant macros
  class MPIComm
  def mpi_comm_world: Rep[MPIComm] = cmacro[MPIComm]("MPI_COMM_WORLD")
  def mpi_max_processor_name: Rep[Int] = cmacro[Int]("MPI_MAX_PROCESSOR_NAME")
  def mpi_comm: Rep[MPIComm] = newStruct[MPIComm]("MPI_Comm")

  class CNull
  def cNull: Rep[CNull] = cmacro[CNull]("NULL")

  def MPI_CHECK(return_value: Rep[Int]) = libFunction[Unit]("MPICHECK", Unwrap(return_value))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  // this is how we deal with library functions (may need pointers)
  def mpi_init() = libFunction[Int]("MPI_Init", Unwrap(cNull), Unwrap(cNull))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  def mpi_finalize() = libFunction[Int]("MPI_Finalize")(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  def mpi_comm_size(world: Rep[MPIComm], size: Var[Int]) =
    libFunction[Int]("MPI_Comm_size", Unwrap(world), UnwrapV(size))(Seq[Int](), Seq(1), Set(1))

  def mpi_comm_rank(world: Rep[MPIComm], rank: Var[Int]) =
    libFunction[Int]("MPI_Comm_rank", Unwrap(world), UnwrapV(rank))(Seq[Int](), Seq(1), Set(1))

  def mpi_get_processor_name(name: Rep[Array[Char]], len: Var[Int]) =
    libFunction[Int]("MPI_Get_processor_name", Unwrap(name), UnwrapV(len))(Seq[Int](), Seq(0, 1), Set(1))

  def mpi_barrier(world: Rep[MPIComm]) =
    libFunction[Int]("MPI_Barrier", Unwrap(world))(Seq(0), Seq[Int](), Set[Int](), Adapter.CTRL)

  def mpi_comm_split(comm: Rep[MPIComm], color: Rep[Int], key: Rep[Int], newcomm: Rep[MPIComm]) =
    libFunction[Int]("MPI_Comm_split", Unwrap(comm), Unwrap(color), Unwrap(key), Unwrap(newcomm))(Seq(0,1,2), Seq(3), Set(3), Adapter.CTRL)

  class MPIDataType
  def mpi_datatype_null: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_DATATYPE_NULL")
  def mpi_char: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_CHAR")
  def mpi_unsigned_char: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UNSIGNED_CHAR")
  def mpi_short: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_SHORT")
  def mpi_unsigned_short: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UNSIGNED_SHORT")
  def mpi_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INT")
  def mpi_unsigned: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UNSIGNED")
  def mpi_long: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LONG")
  def mpi_unsigned_long: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UNSIGNED_LONG")
  def mpi_long_long_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LONG_LONG_INT")
  def mpi_long_long: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LONG_LONG")
  def mpi_float: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_FLOAT")
  def mpi_double: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_DOUBLE")
  def mpi_long_double: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LONG_DOUBLE")
  def mpi_byte: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_BYTE")
  def mpi_wchar: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_WCHAR")
  def mpi_packed: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_PACKED")
  def mpi_lb: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LB")
  def mpi_ub: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UB")
  def mpi_c_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_C_COMPLEX")
  def mpi_c_float_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_C_FLOAT_COMPLEX")
  def mpi_c_double_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_C_DOUBLE_COMPLEX")
  def mpi_c_long_double_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_C_LONG_DOUBLE_COMPLEX")
  def mpi_2int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_2INT")
  def mpi_c_bool: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_C_BOOL")
  def mpi_signed_char: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_SIGNED_CHAR")
  def mpi_unsigned_long_long: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UNSIGNED_LONG_LONG")
  def mpi_character: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_CHARACTER")
  def mpi_integer: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INTEGER")
  def mpi_real: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_REAL")
  def mpi_logical: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LOGICAL")
  def mpi_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_COMPLEX")
  def mpi_double_precision: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_DOUBLE_PRECISION")
  def mpi_2integer: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_2INTEGER")
  def mpi_2real: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_2REAL")
  def mpi_double_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_DOUBLE_COMPLEX")
  def mpi_2double_precision: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_2DOUBLE_PRECISION")
  def mpi_2complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_2COMPLEX")
  def mpi_2double_complex: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_2DOUBLE_COMPLEX")
  def mpi_real2: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_REAL2")
  def mpi_real4: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_REAL4")
  def mpi_complex8: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_COMPLEX8")
  def mpi_real8: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_REAL8")
  def mpi_complex16: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_COMPLEX16")
  def mpi_real16: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_REAL16")
  def mpi_complex32: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_COMPLEX32")
  def mpi_integer1: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INTEGER1")
  def mpi_integer2: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INTEGER2")
  def mpi_integer4: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INTEGER4")
  def mpi_integer8: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INTEGER8")
  def mpi_integer16: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INTEGER16")
  def mpi_complex4: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_COMPLEX4")
  def mpi_int8_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INT8_T")
  def mpi_int16_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INT16_T")
  def mpi_int32t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INT32T")
  def mpi_int64_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_INT64_T")
  def mpi_uint8_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UINT8_T")
  def mpi_uint16_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UINT16_T")
  def mpi_uint32_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UINT32T")
  def mpi_uint64_t: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_UINT64_T")
  def mpi_aint: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_AINT")
  def mpi_offset: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_OFFSET")
  def mpi_float_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_FLOAT_INT")
  def mpi_double_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_DOUBLE_INT")
  def mpi_long_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LONG_INT")
  def mpi_short_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_SHORT_INT")
  def mpi_long_double_int: Rep[MPIDataType] = cmacro[MPIDataType]("MPI_LONG_DOUBLE_INT")

  /** int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm)
   * Input/Output Parameters
      * buffer: starting address of buffer (choice)
   * Input Parameters
      * count:    number of entries in buffer (integer)
      * datatype: data type of buffer (handle)
      * root:     rank of broadcast root (integer)
      * comm:     communicator (handle)
   */
  def mpi_bcast[T:Manifest](buffer: Rep[Array[T]], count: Rep[Int], datatype: Rep[MPIDataType], root: Rep[Int], comm: Rep[MPIComm]) =
    libFunction[Int]("MPI_Bcast", Unwrap(buffer), Unwrap(count), Unwrap(datatype), Unwrap(root), Unwrap(comm))(Seq(0,2,4), Seq(0), Set[Int]())

  // a variation of the function that uses the address as array
  def mpi_bcast_one[T:Manifest](buffer: Rep[T], count: Rep[Int], datatype: Rep[MPIDataType], root: Rep[Int], comm: Rep[MPIComm]) =
    libFunction[Int]("MPI_Bcast", Unwrap(buffer), Unwrap(count), Unwrap(datatype), Unwrap(root), Unwrap(comm))(Seq(0,2,4), Seq(0), Set[Int](0))

  class MPIOp
  def mpi_max = cmacro[MPIOp]("MPI_MAX") // Returns the maximum element.
  def mpi_min = cmacro[MPIOp]("MPI_MIN") // Returns the minimum element.
  def mpi_sum = cmacro[MPIOp]("MPI_SUM") // Sums the elements.
  def mpi_prod = cmacro[MPIOp]("MPI_PROD") // Multiplies all elements.
  def mpi_land = cmacro[MPIOp]("MPI_LAND") // Performs a logical and across the elements.
  def mpi_lor = cmacro[MPIOp]("MPI_LOR") // Performs a logical or across the elements.
  def mpi_band = cmacro[MPIOp]("MPI_BAND") // Performs a bitwise and across the bits of the elements.
  def mpi_bor = cmacro[MPIOp]("MPI_BOR") // Performs a bitwise or across the bits of the elements.
  def mpi_maxloc = cmacro[MPIOp]("MPI_MAXLOC") // Returns the maximum value and the rank of the process that owns it.
  def mpi_minloc = cmacro[MPIOp]("MPI_MINLOC") // Returns the minimum value and the rank of the process that owns it.

  /** int MPI_Allreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
   * Input Parameters:
      * sendbuf: starting address of send buffer (choice)
      * count: number of elements in send buffer (integer)
      * datatype: data type of elements of send buffer (handle)
      * op: operation (handle)
      * comm: communicator (handle)
   * Output Parameters:
      * recvbuf: starting address of receive buffer (choice)
   */
  def mpi_allReduce[T:Manifest](sendbuf: Rep[Array[T]], recvbuf: Rep[Array[T]], count: Rep[Int], dataType: Rep[MPIDataType],
      op: Rep[MPIOp], comm: Rep[MPIComm]) =
    libFunction[Int]("MPI_Allreduce", Unwrap(sendbuf), Unwrap(recvbuf), Unwrap(count), Unwrap(dataType), Unwrap(op),
      Unwrap(comm))(Seq(0, 3, 4, 5), Seq(1), Set[Int]())

  def mpi_in_place = cmacro[Int]("MPI_IN_PLACE")
  def mpi_allReduce_one_inplace[T:Manifest](buf: Rep[T], count: Rep[Int], dataType: Rep[MPIDataType], op: Rep[MPIOp], comm: Rep[MPIComm]) =
    libFunction[Int]("MPI_Allreduce", Unwrap(mpi_in_place), Unwrap(buf), Unwrap(count), Unwrap(dataType), Unwrap(op),
      Unwrap(comm))(Seq(1,3,4,5), Seq(1), Set(1))


  // this is how we bind to library structs with field read access
  class DataStructure1
  // 1. define an implicit value for c-type registration (internally it is a CustomManifest)
  // implicit val registCTypeForDataStructure1 = registerCType[DataStructure1]("DataStructure1")
  // 2. define the instantiation function
  def dataStructure1: Rep[DataStructure1] = newStruct[DataStructure1]("DataStructure1")
  // 3. add any field read access functions
  implicit class DataStructure1Ops(x: Rep[DataStructure1]) {
    def fieldA: Rep[Int] = readField[DataStructure1, Int](x, "fieldA")
  }

  // some example (dummy code). this is using the old way of reflecting new nodes, which requires changes in code gen :)
  // Not recommended to use :)
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

  registerHeader("\"mpi_header.h\"")

  // NOTE: this type map from `mStr.endsWith("$DataStructure1")` to `DataStructure1`
  //       is necessary only if we may use this type without a struct instance,
  //       such as in function parameters.
  override def remap(m: Manifest[_]) = {
    val mStr = m.toString
    if (mStr.endsWith("$DataStructure1")) "DataStructure1"
    else super.remap(m)
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
