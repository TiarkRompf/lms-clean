package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.thirdparty.array_computation.CudaOps

import lms.collection._
import lms.collection.mutable.ArrayTypeLess

object NCCLTypeLess {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import SIZE_TTypeLess._
  import CLibTypeLess._

  class NCCL_RESULT

  def NCCL_DATATYPE(m: Manifest[_])(implicit __pos: SourceContext) = m match {
    case ma if ma == manifest[Int] => CMACRO("ncclInt32", manifest[Int])
    case ma if ma == manifest[Float] => CMACRO("ncclFloat32", manifest[Int])
    case ma => throw new Exception(s"manifest $ma is not yet handled in NCCL_DATATYPE function")
  }

  def NCCL_SUM(implicit __pos: SourceContext) = CMACRO("ncclSum", manifest[Int])
  def NCCL_PROD(implicit __pos: SourceContext) = CMACRO("ncclProd", manifest[Int])
  def NCCL_MIN(implicit __pos: SourceContext) = CMACRO("ncclMin", manifest[Int])
  def NCCL_MAX(implicit __pos: SourceContext) = CMACRO("ncclMax", manifest[Int])

  def NCCL_CHECK(result: TOP)(implicit __pos: SourceContext): UNIT = {
    assert(result.t == manifest[NCCL_RESULT], "NCCL_CHECK must take the NCCL_RESULT type as input")
    UNIT(LIB_FUNCTION(manifest[Unit], "NCCLCHECK", result.x)(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL))
  }


  // Collective Operations
  def NCCL_ALLREDUCE(m: Manifest[_], sendbuf: ARRAY, recvbuf: ARRAY, count: SIZE_T, op: TOP, comm: TOP, stream: TOP)(implicit __pos: SourceContext) = {
    val dataType = NCCL_DATATYPE(m)
    LIB_FUNCTION(manifest[NCCL_RESULT], "ncclAllReduce", sendbuf.x, recvbuf.x, count.x, dataType.x, op.x, comm.x, stream.x)(Seq(0,3,4,5,6), Seq(1,5,6), Set[Int]())
  }

  def NCCL_BROADCAST(m: Manifest[_], sendbuf: ARRAY, recvbuf: ARRAY, count: SIZE_T, root: INT, comm: TOP, stream: TOP)(implicit __pos: SourceContext) = {
    val dataType = NCCL_DATATYPE(m)
    LIB_FUNCTION(manifest[NCCL_RESULT], "ncclBroadCast", sendbuf.x, recvbuf.x, count.x, dataType.x, root.x, comm.x, stream.x)(Seq(0,3,5,6), Seq(1,5,6), Set[Int]())
  }

  def NCCL_REDUCE(m: Manifest[_], sendbuf: ARRAY, recvbuf: ARRAY, count: SIZE_T, op: TOP, root: INT, comm: TOP, stream: TOP)(implicit __pos: SourceContext) = {
    val dataType = NCCL_DATATYPE(m)
    LIB_FUNCTION(manifest[NCCL_RESULT], "ncclReduce", sendbuf.x, recvbuf.x, count.x, dataType.x, op.x, root.x, comm.x, stream.x)(Seq(0,3,4,6,7), Seq(1,6,7), Set[Int]())
  }

  def NCCL_ALLGATHER(m: Manifest[_], sendbuf: ARRAY, recvbuf: ARRAY, sendcount: SIZE_T, comm: TOP, stream: TOP)(implicit __pos: SourceContext) = {
    val dataType = NCCL_DATATYPE(m)
    LIB_FUNCTION(manifest[NCCL_RESULT], "ncclAllGather", sendbuf.x, recvbuf.x, sendcount.x, dataType.x, comm.x, stream.x)(Seq(0,3,4,5), Seq(1,4,5), Set[Int]())
  }


  // Point-to-Point Operations
  def NCCL_SEND(m: Manifest[_], sendbuf: ARRAY, count: SIZE_T, peer: INT, comm: TOP, stream: TOP)(implicit __pos: SourceContext) = {
    val dataType = NCCL_DATATYPE(m)
    // FIXME(feiw): If I add r/w effect on `comm` and `stream`, I lose some inlining. How to fix?
    LIB_FUNCTION(manifest[NCCL_RESULT], "ncclSend", sendbuf.x, count.x, dataType.x, peer.x, comm.x, stream.x)(Seq(0,2), Seq[Int](), Set[Int]())
    // LIB_FUNCTION(manifest[NCCL_RESULT], "ncclSend", sendbuf.x, count.x, dataType.x, peer.x, comm.x, stream.x)(Seq(0,2,4,5), Seq(4,5), Set[Int]())
  }

  def NCCL_RECV(m: Manifest[_], recvbuf: ARRAY, count: SIZE_T, peer: INT, comm: TOP, stream: TOP)(implicit __pos: SourceContext) = {
    val dataType = NCCL_DATATYPE(m)
    LIB_FUNCTION(manifest[NCCL_RESULT], "ncclRecv", recvbuf.x, count.x, dataType.x, peer.x, comm.x, stream.x)(Seq(2,4,5), Seq(0,4,5), Set[Int]())
  }


  // FIXME(feiw) The effect of NCCL_GROUP_START and NCCL_GROUP_END are interesting ones:
  // 1. The order of them should never be changed
  // 2. Other nccl calls cannot swap order with NCCL_GROUP_START or NCCL_GROUP_END
  // 3. NCCL_GROUP_START and NCCL_GROUP_END can be removed if there is no other NCCL calls in between, but they must be removed in pairs
  // I am not sure what kind of read/write effect can we use to achive this dependency pattern
  // For now, let's make sure that the NCCL_GROUP_START/END are called within NCCL_CHECK, which has CTRL effect
  def NCCL_GROUP_START(implicit pos: SourceContext) = CMACRO("ncclGroupStart()", manifest[NCCL_RESULT])
  def NCCL_GROUP_END(implicit pos: SourceContext) = CMACRO("ncclGroupEnd()", manifest[NCCL_RESULT])
  // def NCCL_GROUP_START(implicit pos: SourceContext) =
  //   LIB_FUNCTION(manifest[NCCL_RESULT], "ncclGroupStart")(Seq[Int](), Seq[Int](), Set[Int]())
  // def NCCL_GROUP_END(implicit pos: SourceContext) =
  //   LIB_FUNCTION(manifest[NCCL_RESULT], "ncclGroupEnd")(Seq[Int](), Seq[Int](), Set[Int]())

  // Point-to-Point Operations in Collection
  /**
   * Scatter
   * ncclGroupStart();
   * if (rank == root) {
   *   for (int r=0; r<nranks; r++)
   *     ncclSend(sendbuff[r], size, type, r, comm, stream);
   * }
   * ncclRecv(recvbuff, size, type, root, comm, stream);
   * ncclGroupEnd();
   */
  // def NCCL_SCATTER(m: Manifest[_], sendbufs: ARRAY, recvbuf: ARRAY, ???)

}

trait NCCLOps extends CLibs with SizeTOps with CudaOps {

  /* LMS support for NCCL library */

  class ncclUniqueId
  def ncclUniqueId: Rep[ncclUniqueId] = newStruct[ncclUniqueId]("ncclUniqueId")

  class ncclResultT
  def ncclResult: Rep[ncclResultT] = newStruct[ncclResultT]("ncclResult_t")

  class ncclCommT
  def ncclComm: Rep[ncclCommT] = newStruct[ncclCommT]("ncclComm_t")

  // ncclResult_t ncclGetVersion(int* version)
  def ncclGetVersion(version: Var[Int]) = libFunction[ncclResultT]("ncclGetVersion",
    UnwrapV(version))(Seq[Int](),Seq(0),Set[Int](0))

  // ncclResult_t ncclGetUniqueId(ncclUniqueId* uniqueId)
  def ncclGetUniqueId(id: Rep[ncclUniqueId]): Rep[ncclResultT] =
    libFunction[ncclResultT]("ncclGetUniqueId", Unwrap(id))(Seq(0), Seq(0), Set(0))

  // ncclResult_t ncclCommInitRank(ncclComm_t* comm, int nranks, ncclUniqueId commId, int rank)
  def ncclCommInitRank(comm: Rep[ncclCommT], nranks: Rep[Int], commId: Rep[ncclUniqueId], rank: Rep[Int]) =
    libFunction[ncclResultT]("ncclCommInitRank", Unwrap(comm), Unwrap(nranks), Unwrap(commId),
      Unwrap(rank))(Seq(2), Seq[Int](), Set(0))
      // Unwrap(rank))(Seq(0, 2), Seq(0), Set(0))

  // ncclResult_t ncclCommInitAll(ncclComm_t* comms, int ndev, const int* devlist)
  def ncclCommInitAll(comm: Rep[ncclCommT], ndev: Rep[Int], devlist: Rep[Array[Int]]) =
    libFunction[ncclResultT]("ncclCommInitAll", Unwrap(comm), Unwrap(ndev),
      Unwrap(devlist))(Seq(0,2), Seq(0), Set(0))

  def ncclUniqueIdBytes: Rep[Int] = cmacro[Int]("NCCL_UNIQUE_ID_BYTES")

  def ncclCheck(res: Rep[ncclResultT]) =
    libFunction[Unit]("NCCLCHECK", Unwrap(res))(Seq[Int](), Seq[Int](), Set[Int](), Adapter.CTRL)

  // ncclResult_t ncclCommDestroy(ncclComm_t comm)
  def ncclCommDestroy(comm: Rep[ncclCommT]) =
    libFunction[ncclResultT]("ncclCommDestroy", Unwrap(comm))(Seq(0), Seq(0), Set[Int]())

  // ncclResult_t ncclCommAbort(ncclComm_t comm)
  def ncclCommAbort(comm: Rep[ncclCommT]) = libFunction[ncclResultT]("ncclCommAbort", Unwrap(comm))(Seq(0), Seq(0), Set[Int]())

  // ncclResult_t ncclCommGetAsyncError(ncclComm_t comm, ncclResult_t* asyncError)
  def ncclCommGetAsyncError(comm: Rep[ncclCommT], asyncError: Var[ncclResultT]) =
    libFunction[ncclResultT]("ncclCommGetAsyncError", Unwrap(comm), UnwrapV(asyncError))(Seq(0), Seq(0,1), Set(1))

  // ncclResult_t ncclCommCount(const ncclComm_t comm, int* count)
  def ncclCommCount(comm: Rep[ncclCommT], count: Var[Int]) =
    libFunction[ncclResultT]("ncclCommCount", Unwrap(comm), UnwrapV(count))(Seq(0), Seq(0,1), Set(1))

  // ncclResult_t ncclCommCuDevice(const ncclComm_t comm, int* device)
  def ncclCommCuDevice(comm: Rep[ncclCommT], device: Var[Int]) =
    libFunction[ncclResultT]("ncclCommCuDevice", Unwrap(comm), UnwrapV(device))(Seq(0), Seq(0,1), Set(1))

  // ncclResult_t ncclCommUserRank(const ncclComm_t comm, int* rank)
  def ncclCommUserRank(comm: Rep[ncclCommT], rank: Var[Int]) =
    libFunction[ncclResultT]("ncclCommUserRank", Unwrap(comm), UnwrapV(rank))(Seq(0), Seq(0,1), Set(1))

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
  def ncclAllReduce[T:Manifest](sendbuf: Rep[Array[T]], recvbuf: Rep[Array[T]], count: Rep[Int],
      dataType: Rep[ncclDataTypeT], op: Rep[ncclRedOpT], comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclAllReduce", Unwrap(sendbuf), Unwrap(recvbuf), Unwrap(count), Unwrap(dataType),
      Unwrap(op), Unwrap(comm), Unwrap(stream))(Seq(0,3,4,5,6), Seq(1,5,6), Set[Int]())

  /**
   * ncclResult_t ncclBroadcast(const void* sendbuff, void* recvbuff, size_t count, ncclDataType_t datatype, int root, ncclComm_t comm, cudaStream_t stream)
   * Copies count elements from sendbuff on the root` rank to all ranks' ``recvbuff. sendbuff is only used on rank root and ignored for other ranks.
   * In-place operation will happen if sendbuff == recvbuff.
   */
  def ncclBroadcast[T:Manifest](sendbuff: Rep[Array[T]], recvbuff: Rep[Array[T]], count: Rep[Int],
      dataType: Rep[ncclDataTypeT], root: Rep[Int], comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclBroadCast", Unwrap(sendbuff), Unwrap(recvbuff), Unwrap(count), Unwrap(dataType),
      Unwrap(root), Unwrap(comm), Unwrap(stream))(Seq(0,3,5,6), Seq(1,5,6), Set[Int]())

  /**
   * ncclResult_t ncclReduce(const void* sendbuff, void* recvbuff, size_t count, ncclDataType_t datatype, ncclRedOp_t op, int root, ncclComm_t comm, cudaStream_t stream)
   * Reduce data arrays of length count in sendbuff into recvbuff on the root rank using the op operation. recvbuff is only used on rank root and ignored for other ranks.
   * In-place operation will happen if sendbuff == recvbuff.
   */
  def ncclReduce[T:Manifest](sendbuff: Rep[Array[T]], recvbuff: Rep[Array[T]], count: Rep[Int], datatype: Rep[ncclDataTypeT],
      op: Rep[ncclRedOpT], root: Rep[Int], comm: Rep[ncclCommT], stream:Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclReduce", Unwrap(sendbuff), Unwrap(recvbuff), Unwrap(count), Unwrap(datatype),
      Unwrap(op), Unwrap(root), Unwrap(comm), Unwrap(stream))(Seq(0,3,4,6,7), Seq(1,6,7), Set[Int]())

  /**
   * ncclResult_t ncclAllGather(const void* sendbuff, void* recvbuff, size_t sendcount, ncclDataType_t datatype, ncclComm_t comm, cudaStream_t stream)
   * Gather sendcount values from all GPUs into recvbuff, receiving data from rank i at offset i*sendcount.
   * Note : This assumes the receive count is equal to nranks*sendcount, which means that recvbuff should have a size of at least nranks*sendcount elements.
   * In-place operation will happen if sendbuff == recvbuff + rank * sendcount.
   */
  def ncclAllGather[T:Manifest](sendbuff: Rep[Array[T]], recvbuff: Rep[Array[T]], sendcount: Rep[Int], datatype: Rep[ncclDataTypeT],
      comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclAllGather", Unwrap(sendbuff), Unwrap(recvbuff), Unwrap(sendcount), Unwrap(datatype), Unwrap(comm),
      Unwrap(stream))(Seq(0,3,4,5), Seq(1,4,5), Set[Int]())

  /**
   * ncclResult_t ncclReduceScatter(const void* sendbuff, void* recvbuff, size_t recvcount, ncclDataType_t datatype, ncclRedOp_t op, ncclComm_t comm, cudaStream_t stream)
   * Reduce data in sendbuff from all GPUs using the op operation and leave the reduced result scattered over the devices so that the recvbuff on rank i will contain the i-th block of the result.
   * Note: This assumes the send count is equal to nranks*recvcount, which means that sendbuff should have a size of at least nranks*recvcount elements.
   */
  def ncclReduceScatter[T:Manifest](sendbuff: Rep[Array[T]], recvbuff: Rep[Array[T]], recvcount: Rep[Int], datatype: Rep[ncclDataTypeT],
      op: Rep[ncclRedOpT], comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclReduceScatter", Unwrap(sendbuff), Unwrap(recvbuff), Unwrap(recvcount), Unwrap(datatype),
      Unwrap(op), Unwrap(comm), Unwrap(stream))(Seq(0,3,4,5,6), Seq(1,5,6), Set[Int]())


  /**
   * NCCL calls are associated to a stream and are passed as the last argument of the collective communication function.
   * The NCCL call returns when the operation has been effectively enqueued to the given stream, or returns an error.
   * The collective operation is then executed asynchronously on the CUDA device.
   * The operation status can be queried using standard CUDA semantics, for example, calling cudaStreamSynchronize or using CUDA events.
   */

  /**
   * The group semantics can also be used to have multiple collective operations performed within a single NCCL launch.
   * This is useful for reducing the launch overhead, in other words, latency, as it only occurs once for multiple operations.
   * Init functions cannot be aggregated with other init functions, nor with communication functions.
   *
   * example:
   * ncclGroupStart();
   * ncclBroadcast(sendbuff1, recvbuff1, count1, datatype, root, comm, stream);
   * ncclAllReduce(sendbuff2, recvbuff2, count2, datatype, comm, stream);
   * ncclAllReduce(sendbuff3, recvbuff3, count3, datatype, comm, stream);
   * ncclGroupEnd();
   */

  /**
   * ncclResult_t ncclGroupStart()
   * Start a group call.
   * All subsequent calls to NCCL may not block due to inter-CPU synchronization.
   */
  def ncclGroupStart() = libFunction[ncclResultT]("ncclGroupStart")(Seq[Int](), Seq[Int](), Set[Int]())

  /**
   * ncclResult_t ncclGroupEnd()
   * End a group call.
   * Returns when all operations since ncclGroupStart have been processed.
   *   This means communication primitives have been enqueued to the provided streams, but are not necessary complete.
   * When used with the ncclCommInitRank call, the ncclGroupEnd call waits for all communicators to be initialized.
   * Note: There is a maximum of 2048 NCCL operations that can be inserted between the ncclGroupStart and ncclGroupEnd calls.
   *   If this limit is exceeded, then a warning message will be emitted and the NCCL operation will return a failure code.
   */
  def ncclGroupEnd() = libFunction[ncclResultT]("ncclGroupEnd")(Seq[Int](), Seq[Int](), Set[Int]())

  /**
   * (Since NCCL 2.7) Point-to-point communication can be used to express any communication pattern between ranks.
   * Any point-to-point communication needs two NCCL calls : a call to ncclSend() on one rank and a corresponding ncclRecv() on the other rank,
   * with the same count and data type.
   */

  /**
   * ncclResult_t ncclSend(const void* sendbuff, size_t count, ncclDataType_t datatype, int peer, ncclComm_t comm, cudaStream_t stream)
   * Send data from sendbuff to rank peer.
   * Rank peer needs to call ncclRecv with the same datatype and the same count from this rank.
   * This operation is blocking for the GPU. If multiple ncclSend() and ncclRecv() operations need to progress concurrently to complete,
   * they must be fused within a ncclGroupStart()/ ncclGroupEnd() section.
   */
  def ncclSend[T:Manifest](sendbuff: Rep[Array[T]], count: Rep[Int], datatype: Rep[ncclDataTypeT], peer: Rep[Int],
      comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclSend", Unwrap(sendbuff), Unwrap(count), Unwrap(datatype), Unwrap(peer), Unwrap(comm),
      Unwrap(stream))(Seq(0,2,4,5), Seq(4,5), Set[Int]())

  /**
   * ncclResult_t ncclRecv(void* recvbuff, size_t count, ncclDataType_t datatype, int peer, ncclComm_t comm, cudaStream_t stream)
   * Receive data from rank peer into recvbuff.
   * Rank peer needs to call ncclSend with the same datatype and the same count to this rank.
   * This operation is blocking for the GPU. If multiple ncclSend() and ncclRecv() operations need to progress concurrently to complete,
   *   they must be fused within a ncclGroupStart()/ ncclGroupEnd() section.
   */
  def ncclRecv[T:Manifest](recvbuff: Rep[Array[T]], count: Rep[Int], datatype: Rep[ncclDataTypeT], peer: Rep[Int],
      comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) =
    libFunction[ncclResultT]("ncclRecv", Unwrap(recvbuff), Unwrap(count), Unwrap(datatype), Unwrap(peer),
      Unwrap(comm), Unwrap(stream))(Seq(2,4,5), Seq(0,4,5), Set[Int]())

  /**
   * Thread Safety
   * NCCL primitives are generally not thread-safe, however, they are reentrant. Multiple threads should use separate communicator objects.
   */


  /**
   * Sendrecv
   * Exchange data between two ranks, both sending and receiving at the same time
   */
  def ncclSendRecv[T:Manifest](sendbuff: Rep[Array[T]], recvbuff: Rep[Array[T]], count: Rep[Int], datatype: Rep[ncclDataTypeT], peer: Rep[Int],
      comm: Rep[ncclCommT], stream: Rep[cudaStreamT]) = {
    ncclCheck(ncclGroupStart())
    ncclCheck(ncclSend(sendbuff, count, datatype, peer, comm, stream))
    ncclCheck(ncclRecv(recvbuff, count, datatype, peer, comm, stream))
    ncclCheck(ncclGroupEnd())
  }

  /**
   * One-to-all (scatter)
   * A one-to-all operation from a root rank can be expressed by merging all send and receive operations in a group
   * `root` is the root rank, `myRank` is the rank of current device, `nRanks` is the number of devices associated with
   * the communicator `comm`.
   */
  def ncclScatter[T:Manifest](sendbuff: Rep[Array[Array[T]]], recvbuff: Rep[Array[T]], root: Rep[Int], myRank: Rep[Int], nRanks: Rep[Int],
      count: Rep[Int], datatype: Rep[ncclDataTypeT], comm: Rep[ncclCommT], stream: Rep[cudaStreamT])(implicit __pos: SourceContext) = {
    ncclCheck(ncclGroupStart())
    __ifThenElse(equals(myRank, root), {
      for (i <- (0 until nRanks): Rep[Range]) {
        ncclCheck(ncclSend(sendbuff(i), count, datatype, i, comm, stream))
      }
    }, {})
    ncclCheck(ncclRecv(recvbuff, count, datatype, root, comm, stream))
    ncclCheck(ncclGroupEnd())
  }

  /**
   * All-to-one (gather)
   * Similar to One-to-all (scatter)
   */
  def ncclGather[T:Manifest](sendbuff: Rep[Array[T]], recvbuff: Rep[Array[Array[T]]], root: Rep[Int], myRank: Rep[Int], nRanks: Rep[Int],
        count: Rep[Int], datatype: Rep[ncclDataTypeT], comm: Rep[ncclCommT], stream: Rep[cudaStreamT])(implicit __pos: SourceContext) = {
    ncclCheck(ncclGroupStart())
    __ifThenElse(equals(myRank, root), {
      for (i <- (0 until nRanks): Rep[Range]) {
        ncclCheck(ncclRecv(recvbuff(i), count, datatype, i, comm, stream))
      }
    }, {})
    ncclCheck(ncclSend(sendbuff, count, datatype, root, comm, stream))
    ncclCheck(ncclGroupEnd())
  }

  /**
   * All-to-all
   * A merged loop of send/recv operations to/from all peers
   */
  def ncclAll2All[T:Manifest](sendbuff: Rep[Array[Array[T]]], recvbuff: Rep[Array[Array[T]]], nRanks: Rep[Int], count: Rep[Int],
      datatype: Rep[ncclDataTypeT], comm: Rep[ncclCommT], stream: Rep[cudaStreamT])(implicit __pos: SourceContext) = {
    ncclCheck(ncclGroupStart())
    for (i <- (0 until nRanks): Rep[Range]) {
      ncclCheck(ncclSend(sendbuff(i), count, datatype, i, comm, stream))
      ncclCheck(ncclRecv(recvbuff(i), count, datatype, i, comm, stream))
    }
    ncclCheck(ncclGroupEnd())
  }
}

trait CCodeGenNCCLOps extends CCodeGenSizeTOps with CCodeGenLibs {
  override def remap(m: Manifest[_]): String = m.runtimeClass.getName match {
    case s: String if s.endsWith("ncclResultT") => "ncclResult_t"
    case s: String if s.endsWith("ncclDataTypeT") => "ncclDataType_t"
    case _ => super.remap(m)
  }
}
