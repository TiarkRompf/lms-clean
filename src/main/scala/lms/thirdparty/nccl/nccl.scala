package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._

trait NCCLOps extends CLibs with SizeTOps with CudaOps {

  /* LMS support for NCCL library */

  class ncclUniqueId
  def ncclUniqueId: Rep[ncclUniqueId] = newStruct[ncclUniqueId]("ncclUniqueId")

  class ncclResultT
  def ncclResult: Rep[ncclResultT] = newStruct[ncclResultT]("ncclResult_t")

  class ncclCommT
  def ncclComm: Rep[ncclCommT] = newStruct[ncclCommT]("ncclComm_t")

  def ncclUniqueIdBytes: Rep[Int] = cmacro[Int]("NCCL_UNIQUE_ID_BYTES")

  def ncclCheck(res: Rep[ncclResultT]) =
    libFunction[Unit]("NCCLCHECK", Unwrap(res))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  def ncclGetUniqueId(id: Rep[ncclUniqueId]): Rep[ncclResultT] =
    libFunction[ncclResultT]("ncclGetUniqueId", Unwrap(id))(Seq(0), Seq(0), Set(0))

  def ncclCommInitRank(comm: Rep[ncclCommT], size: Rep[Int], commId: Rep[ncclUniqueId], rank: Rep[Int]) =
    libFunction[ncclResultT]("ncclCommInitRank", Unwrap(comm), Unwrap(size), Unwrap(commId), Unwrap(rank))(Seq(0,2), Seq(0), Set(0))

  // ncclResult_t ncclCommDestroy(ncclComm_t comm)
  def ncclCommDestroy(comm: Rep[ncclCommT]) =
    libFunction[ncclResultT]("ncclCommDestroy", Unwrap(comm))(Seq(0), Seq(0), Set[Int]())

  class ncclDataTypeT // NCCL defines the following integral and floating data-types.
  def ncclInt8 = cmacro[ncclDataTypeT]("ncclInt8") // Signed 8-bits integer
  def ncclChar = cmacro[ncclDataTypeT]("ncclChar") // Signed 8-bits integer
  def ncclUint8 = cmacro[ncclDataTypeT]("ncclUint8") // Unsigned 8-bits integer
  def ncclInt32 = cmacro[ncclDataTypeT]("ncclInt32") // Signed 32-bits integer
  def ncclInt = cmacro[ncclDataTypeT]("ncclInt") // Signed 32-bits integer
  def ncclUint32 = cmacro[ncclDataTypeT]("ncclUint32") // Unsigned 32-bits integer
  def ncclInt64 = cmacro[ncclDataTypeT]("ncclInt64") // Signed 64-bits integer
  def ncclUint64 = cmacro[ncclDataTypeT]("ncclUint64") // Unsigned 64-bits integer
  def ncclFloat16 = cmacro[ncclDataTypeT]("ncclFloat16") // 16-bits floating point number (half precision)
  def ncclHalf = cmacro[ncclDataTypeT]("ncclHalf") // 16-bits floating point number (half precision)
  def ncclFloat32 = cmacro[ncclDataTypeT]("ncclFloat32") // 32-bits floating point number (single precision)
  def ncclFloat = cmacro[ncclDataTypeT]("ncclFloat") // 32-bits floating point number (single precision)
  def ncclFloat64 = cmacro[ncclDataTypeT]("ncclFloat64") // 64-bits floating point number (double precision)
  def ncclDouble = cmacro[ncclDataTypeT]("ncclDouble") // 64-bits floating point number (double precision)

  class ncclRedOpT // Defines the reduction operation.
  def ncclSum = cmacro[ncclRedOpT]("ncclSum") // Perform a sum (+) operation
  def ncclProd = cmacro[ncclRedOpT]("ncclProd") // Perform a product (*) operation
  def ncclMin = cmacro[ncclRedOpT]("ncclMin") // Perform a min operation
  def ncclMax = cmacro[ncclRedOpT]("ncclMax") // Perform a max operation

  /**
   * ncclResult_t ncclAllReduce(const void* sendbuff, void* recvbuff, size_t count, ncclDataType_t datatype, ncclRedOp_t op, ncclComm_t comm, cudaStream_t stream)
   * Reduce data arrays of length count in sendbuff using op operation and leaves identical copies of the result on each recvbuff.
   * In-place operation will happen if sendbuff == recvbuff.
   */
   def ncclAllReduce[T:Manifest](sendbuf: Rep[Array[T]], recvbuf: Rep[Array[T]], count: Rep[SizeT],
       dataType: Rep[ncclDataTypeT], op: Rep[ncclRedOpT], comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
     libFunction[ncclResultT]("ncclAllReduce", Unwrap(sendbuf), Unwrap(recvbuf), Unwrap(count), Unwrap(dataType),
       Unwrap(op), Unwrap(comm), Unwrap(stream))(Seq(0,3,4,5,6), Seq(1,5,6), Set[Int]())





}


