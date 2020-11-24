package lms
package thirdparty

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
// import lms.collection.mutable.ArrayTypeLess
import lms.collection.mutable._
import lms.thirdparty.array_computation.{CUDATypeLess, CCodeGenCudaOps, CudaOps}


class NCCLTest extends TutorialFunSuite {
  val under = "thirdparty/nccl/"

  abstract class DslDriverCNCCL[A: Manifest, B: Manifest] extends DslDriverC[A,B] with NCCLOps with CudaOps with SizeTOps with MPIOps with ArrayOps{ q =>
    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCudaOps with CCodeGenSizeTOps with CCodeGenMPI with CCodeGenNCCLOps {
      val IR: q.type = q

      registerHeader("\"nccl_header.h\"")
      // registerHeader("<nccl.h>")
    }
    override val compilerCommand = "nvcc -std=c++11 -O3"

    val curPath = System.getProperty("user.dir")
    override val sourceFile = s"$curPath/snippet.cu"
    override val executable = s"$curPath/snippet"
  }


  test("sanity-check") {
    // just sanity check. Output code not runnable
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        val comms = ncclComm
        val devs = Array(0)
        ncclCheck(ncclCommInitAll(comms, 1, devs))

        val s = cudaStream
        cudaStreamCreate(s)
        var sendbuff = cudaMalloc2[Float](0)
        var recvbuff = cudaMalloc2[Float](0)
        ncclCheck(ncclAllReduce(sendbuff, recvbuff, SizeT(0), ncclFloat, ncclSum, comms, s))
        cudaCall(cudaStreamSynchronize(s))
        printf("end")
      }
    }
    check("sanity-check", driver.code, "cu")
  }

  test("single-process-single-device") {
    // based on
    // https://docs.nvidia.com/deeplearning/nccl/user-guide/docs/examples.html
    // example 1, but single device (nDev = 1)
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {

        val comms = ncclComm

        // managing one device
        val nDev = 1
        val size = 32*1024*1024
        val devs = Array(0)

        val s = cudaStream

        cudaCall(cudaSetDevice(0))
        var sendbuff = cudaMalloc2[Float](size)
        var recvbuff = cudaMalloc2[Float](size)
        cudaCall(cudaMemset2[Float](sendbuff, 1, size))
        cudaCall(cudaMemset2[Float](recvbuff, 0, size))
        cudaCall(cudaStreamCreate(s))

        // initializing NCCL
        ncclCheck(ncclCommInitAll(comms, nDev, devs))

        // calling NCCL communication API
        ncclCheck(ncclAllReduce(sendbuff, recvbuff, SizeT(size), ncclFloat, ncclSum, comms, s))

        // synchronizing on CUDA streams to wait form completion of NCCL operation
        cudaCall(cudaSetDevice(0))
        cudaCall(cudaStreamSynchronize(s))

        // free device buffers
        cudaCall(cudaFree[Float](sendbuff))
        cudaCall(cudaFree[Float](recvbuff))

        // finalizing NCCL
        ncclCheck(ncclCommDestroy(comms))

        printf("Success \n")
      }
    }
    check("single-process-single-device", driver.code, "cu")
  }


  test("one-device-per-process") {
    // based on
    // https://docs.nvidia.com/deeplearning/nccl/user-guide/docs/examples.html
    // example 2
    val driver = new DslDriverCNCCL[Int, Unit] {

      @virtualize
      def snippet(arg:Rep[Int]) = {
        val size = 32*1024*1024
        var myRank = 0
        var nRanks = 0
        val localRank = 0

        // initializing MPI
        MPI_CHECK(mpi_init())
        MPI_CHECK(mpi_comm_rank(mpi_comm_world, myRank))
        MPI_CHECK(mpi_comm_size(mpi_comm_world, nRanks))

        // calculating localRank
        // for now, localRank = 0

        // get NCCL unique ID at rank 0 and broadcast it to all others
        var id = ncclUniqueId
        if (myRank == 0) {
          ncclCheck(ncclGetUniqueId(id))
        }
        MPI_CHECK(mpi_bcast_one(id, ncclUniqueIdBytes, mpi_byte, 0, mpi_comm_world))

        // picking a GPU based on localRank, allocate device buffers
        cudaCall(cudaSetDevice(myRank))
        var sendbuff = cudaMalloc2[Float](size)
        var recvbuff = cudaMalloc2[Float](size)
        val s = cudaStream
        cudaCall(cudaStreamCreateWithFlags(s, cudaStreamDefault))

        // initializing NCCL
        val comm = ncclComm
        ncclCheck(ncclCommInitRank(comm, nRanks, id, myRank))

        // communicating using NCCL
        ncclCheck(ncclAllReduce(sendbuff, recvbuff, SizeT(size), ncclFloat, ncclSum, comm, s))

        // completing NCCL operation by synchronizing on the CUDA stream
        cudaCall(cudaStreamSynchronize(s))

        // free device buffers
        cudaCall(cudaFree(sendbuff))
        cudaCall(cudaFree(recvbuff))

        // finalizing NCCL
        ncclCheck(ncclCommDestroy(comm))

        // finalizing MPI
        MPI_CHECK(mpi_finalize())

        printf("[MPI Rank %d] Success \n", myRank)
      }
    }
    check("one-device-per-process", driver.code, "cu")
  }

  test("mpi-reduce") {
    // based on file: main/scala/lms/thirdparty/nccl/cu_examples/mpi/mpi_test.cu
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        val SIZE = 128
        val NITERS = 1

        val commId = ncclUniqueId
        var size = 0
        var rank = 0

        MPI_CHECK(mpi_init())
        MPI_CHECK(mpi_comm_rank(mpi_comm_world, rank))
        MPI_CHECK(mpi_comm_size(mpi_comm_world, size))

        if (arg < size) {
          if (rank == 0) {
            printf("Usage : %s <GPU list per rank\n", arg)
          }
          exit(1)
        }

        // We have to set our device before NCCL init
        // change to atoi(argv[rank+1]) later
        val gpu = 0
        cudaCall(cudaSetDevice(rank))
        MPI_CHECK(mpi_barrier(mpi_comm_world))

        // NCCL Communicator creation
        val comm = ncclComm
        ncclCheck(ncclGetUniqueId(commId))
        MPI_CHECK(mpi_bcast_one(commId, ncclUniqueIdBytes, mpi_char, 0, mpi_comm_world))
        ncclCheck(ncclCommInitRank(comm, size, commId, rank))

        // cuda stream creation
        val stream = cudaStream
        cudaCall(cudaStreamCreateWithFlags(stream, cudaStreamNonBlocking))

        // initialize input values
        // split dptr to dptr_send and dptr_recv to avoid pointer addition
        val dptr_send = cudaMalloc2[Int](SIZE)
        val dptr_recv = cudaMalloc2[Int](SIZE)
        val value = NewArray[Int](SIZE)
        for (i <- (0 until SIZE): Rep[Range]) {
          value(i) = rank + 1
        }
        cudaCall(cudaMemcpy[Int](dptr_send, value, SizeT(SIZE), host2device))

        // compute final value
        val ref = size * (size + 1) / 2

        // run allreduce
        var errors = 0
        for (i <- (0 until NITERS): Rep[Range]) {
          ncclCheck(ncclAllReduce(dptr_send, dptr_recv, SizeT(SIZE), ncclInt, ncclSum, comm, stream))
        }

        // Check results
        cudaCall(cudaStreamSynchronize(stream))
        cudaCall(cudaMemcpy[Int](value, dptr_recv, SizeT(SIZE), device2host))
        for (i <- (0 until SIZE): Rep[Range]) {
          if (value(i) != ref) {
            errors = errors + 1
            printf("error")
          }
        }

        cudaCall(cudaFree(dptr_recv))
        cudaCall(cudaFree(dptr_send))

        MPI_CHECK(mpi_allReduce_one_inplace(errors, 1, mpi_integer, mpi_sum, mpi_comm_world))
        if (rank == 0) {
          if (errors > 0) {
            printf("%d errors. Test FAILED.\n", errors)
          } else {
            printf("test PASSED.\n")
          }
        }
        MPI_CHECK(mpi_finalize())
        ncclCheck(ncclCommDestroy(comm))
      }
    }
    check("mpi-reduce", driver.code, "cu")
  }

  test("p2p-sendrecv") {
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        val size = 1024
        var myRank = 0
        var nRanks = 0

        // initializing MPI
        MPI_CHECK(mpi_init())
        MPI_CHECK(mpi_comm_rank(mpi_comm_world, myRank))
        MPI_CHECK(mpi_comm_size(mpi_comm_world, nRanks))

        printf("myRank: %d, nRanks: %d\n", myRank, nRanks)

        val id = ncclUniqueId
        val comm = ncclComm
        val s = cudaStream

        // get NCCL unique ID at rank 0 and broadcast it to all others
        if (myRank == 0) {
          ncclCheck(ncclGetUniqueId(id))
        }
        MPI_CHECK(mpi_bcast_one(id, ncclUniqueIdBytes, mpi_byte, 0, mpi_comm_world))

        cudaCall(cudaSetDevice(myRank))
        val sendbuff = cudaMalloc2[Float](size)
        val recvbuff = cudaMalloc2[Float](size)
        val values = NewArray[Float](size)
        for (i <- (0 until size): Rep[Range]) {
          values(i) = 2
        }
        cudaCall(cudaMemcpyOfT[Float](sendbuff, values, size, host2device))
        cudaCall(cudaStreamCreateWithFlags(s, cudaStreamDefault))

        // initializing NCCL
        ncclCheck(ncclCommInitRank(comm, nRanks, id, myRank))

        // communicating using NCCL (send-recv)
        val peer = if (myRank == 0) { 1 } else { 0 }
        ncclCheck(ncclGroupStart())
        ncclCheck(ncclSend(sendbuff, SizeT(size), ncclFloat, peer, comm, s))
        ncclCheck(ncclRecv(recvbuff, SizeT(size), ncclFloat, peer, comm, s))
        ncclCheck(ncclGroupEnd())

        cudaCall(cudaStreamSynchronize(s))
        cudaCall(cudaMemcpyOfT[Float](values, recvbuff, size, device2host))

        // checking results
        var errors = 0
        for (i <- (0 until size): Rep[Range]) {
          if (values(0) != 2) {
            errors += 1;
          }
        }

        // free device buffers
        cudaCall(cudaFree(sendbuff))
        cudaCall(cudaFree(recvbuff))

        // finalizing NCCL
        ncclCheck(ncclCommDestroy(comm))

        // finalizing MPI
        MPI_CHECK(mpi_finalize())

        if (errors != 0) {
          printf("[MPI Rank %d] Found %d errors.\n", myRank, errors);
        } else {
          printf("[MPI Rank %d] Success \n", myRank)
        }
      }
    }
    check("p2p-sendrecv", driver.code, "cu")
  }


  test("p2p-scatter") {
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        val size = 1024
        var myRank = 0
        var nRanks = 0

        // initializing MPI
        MPI_CHECK(mpi_init())
        MPI_CHECK(mpi_comm_rank(mpi_comm_world, myRank))
        MPI_CHECK(mpi_comm_size(mpi_comm_world, nRanks))

        printf("myRank: %d, nRanks: %d\n", myRank, nRanks)

        val id = ncclUniqueId
        val comm = ncclComm
        val s = cudaStream

        // get NCCL unique ID at rank 0 and broadcast it to all others
        if (myRank == 0) {
          ncclCheck(ncclGetUniqueId(id))
        }
        MPI_CHECK(mpi_bcast_one(id, ncclUniqueIdBytes, mpi_byte, 0, mpi_comm_world))

        // initialize values as array of 2
        val values = NewArray[Float](size)
        for (i <- (0 until size): Rep[Range]) {
          values(i) = 2
        }
        cudaCall(cudaSetDevice(myRank))
        val sendbuff = NewArray[Array[Float]](nRanks)
        for (i <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaMalloc3[Float](sendbuff(i), size))
          cudaCall(cudaMemcpyOfT[Float](sendbuff(i), values, size, host2device))
        }
        val recvbuff = cudaMalloc2[Float](size)

        cudaCall(cudaStreamCreateWithFlags(s, cudaStreamDefault))

        // initializing NCCL
        ncclCheck(ncclCommInitRank(comm, nRanks, id, myRank))

        // one-to-all (scatter)
        ncclCheck(ncclGroupStart())
        if (myRank == 0) {
          for (i <- (0 until nRanks): Rep[Range]) {
            ncclCheck(ncclSend(sendbuff(i), SizeT(size), ncclFloat, i, comm, s))
          }
        }
        ncclCheck(ncclRecv(recvbuff, SizeT(size), ncclFloat, 0, comm, s))
        ncclCheck(ncclGroupEnd())

        cudaCall(cudaStreamSynchronize(s))
        cudaCall(cudaMemcpyOfT[Float](values, recvbuff, size, device2host))

        // check results
        var errors = 0
        for (i <- (0 until size): Rep[Range]) {
          if (values(0) != 2) {
            errors += 1;
          }
        }

        // free device buffers
        for (i <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaFree(sendbuff(i)))
        }
        cudaCall(cudaFree(recvbuff))

        // finalizing NCCL
        ncclCheck(ncclCommDestroy(comm))

        // finalizing MPI
        MPI_CHECK(mpi_finalize())

        if (errors != 0) {
          printf("[MPI Rank %d] Found %d errors.\n", myRank, errors);
        } else {
          printf("[MPI Rank %d] Success \n", myRank)
        }
      }
    }
    // System.out.println(indent(driver.code))
    check("p2p-scatter", driver.code, "cu")
  }

  test("p2p-gather") {
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        val size = 1024
        var myRank = 0
        var nRanks = 0

        // initializing MPI
        MPI_CHECK(mpi_init())
        MPI_CHECK(mpi_comm_rank(mpi_comm_world, myRank))
        MPI_CHECK(mpi_comm_size(mpi_comm_world, nRanks))

        printf("myRank: %d, nRanks: %d\n", myRank, nRanks)

        val id = ncclUniqueId
        val comm = ncclComm
        val s = cudaStream

        // get NCCL unique ID at rank 0 and broadcast it to all others
        if (myRank == 0) {
          ncclCheck(ncclGetUniqueId(id))
        }
        MPI_CHECK(mpi_bcast_one(id, ncclUniqueIdBytes, mpi_byte, 0, mpi_comm_world))

        // initialize values as array of 2
        val values = NewArray[Float](size)
        for (i <- (0 until size): Rep[Range]) {
          values(i) = 2
        }

        cudaCall(cudaSetDevice(myRank))
        val sendbuff = cudaMalloc2[Float](size)
        cudaCall(cudaMemcpyOfT[Float](sendbuff, values, size, host2device))
        val recvbuff = NewArray[Array[Float]](nRanks)
        for (i <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaMalloc3[Float](recvbuff(i), size))
        }

        cudaCall(cudaStreamCreateWithFlags(s, cudaStreamDefault))

        // initializing NCCL
        ncclCheck(ncclCommInitRank(comm, nRanks, id, myRank))

        // one-to-all (scatter)
        ncclCheck(ncclGroupStart())
        if (myRank == 0) {
          for (i <- (0 until nRanks): Rep[Range]) {
            ncclCheck(ncclRecv(recvbuff(i), SizeT(size), ncclFloat, i, comm, s))
          }
        }
        ncclCheck(ncclSend(sendbuff, SizeT(size), ncclFloat, 0, comm, s))
        ncclCheck(ncclGroupEnd())

        cudaCall(cudaStreamSynchronize(s))

        // check results
        var errors = 0
        if (myRank == 0) {
          for (j <- (0 until nRanks): Rep[Range]) {
            cudaCall(cudaMemcpyOfT[Float](values, recvbuff(j), size, device2host))
            for (i <- (0 until size): Rep[Range]) {
              if (values(i) != 2) {
                errors += 1;
              }
            }
          }
        }

        // free device buffers
        cudaCall(cudaFree(sendbuff))
        for (i <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaFree(recvbuff(i)))
        }

        // finalizing NCCL
        ncclCheck(ncclCommDestroy(comm))

        // finalizing MPI
        MPI_CHECK(mpi_finalize())

        if (errors != 0) {
          printf("[MPI Rank %d] Found %d errors.\n", myRank, errors);
        } else {
          printf("[MPI Rank %d] Success \n", myRank)
        }
      }
    }
    check("p2p-gather", driver.code, "cu")
  }

  test("p2p-all2all") {
    val driver = new DslDriverCNCCL[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        val size = 1024
        var myRank = 0
        var nRanks = 0

        // initializing MPI
        MPI_CHECK(mpi_init())
        MPI_CHECK(mpi_comm_rank(mpi_comm_world, myRank))
        MPI_CHECK(mpi_comm_size(mpi_comm_world, nRanks))

        printf("myRank: %d, nRanks: %d\n", myRank, nRanks)

        var id = ncclUniqueId
        val comm = ncclComm
        val s = cudaStream

        // get NCCL unique ID at rank 0 and broadcast it to all others
        if (myRank == 0) {
          ncclCheck(ncclGetUniqueId(id))
        }
        MPI_CHECK(mpi_bcast_one(id, ncclUniqueIdBytes, mpi_byte, 0, mpi_comm_world))

        // picking a GPU based on myRank

        val values = NewArray[Float](size)
        for (i <- (0 until size): Rep[Range]) {
          values(i) = 2
        }

        cudaCall(cudaSetDevice(myRank))
        val sendbuff = NewArray[Array[Float]](nRanks)
        val recvbuff = NewArray[Array[Float]](nRanks)
        for (i <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaMalloc3[Float](sendbuff(i), size))
          cudaCall(cudaMalloc3[Float](recvbuff(i), size))
          cudaCall(cudaMemcpyOfT[Float](sendbuff(i), values, size, host2device))
        }

        cudaCall(cudaStreamCreateWithFlags(s, cudaStreamDefault))

        // initializing NCCL
        ncclCheck(ncclCommInitRank(comm, nRanks, id, myRank))

        // all-to-all
        ncclCheck(ncclGroupStart())
        for (i <- (0 until nRanks): Rep[Range]) {
          ncclCheck(ncclSend(sendbuff(i), SizeT(size), ncclFloat, i, comm, s))
          ncclCheck(ncclRecv(recvbuff(i), SizeT(size), ncclFloat, i, comm, s))
        }
        ncclCheck(ncclGroupEnd())

        cudaCall(cudaStreamSynchronize(s))

        // check results
        var errors = 0
        for (j <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaMemcpyOfT[Float](values, recvbuff(j), size, device2host))
          for (i <- (0 until size): Rep[Range]) {
            if (values(i) != 2) {
              errors += 1;
            }
          }
        }

        // free device buffers
        for (i <- (0 until nRanks): Rep[Range]) {
          cudaCall(cudaFree(sendbuff(i)))
          cudaCall(cudaFree(recvbuff(i)))
        }

        // finalizing NCCL
        ncclCheck(ncclCommDestroy(comm))

        // finalizing MPI
        MPI_CHECK(mpi_finalize())

        if (errors != 0) {
          printf("[MPI Rank %d] Found %d errors.\n", myRank, errors);
        } else {
          printf("[MPI Rank %d] Success \n", myRank)
        }
      }
    }
    check("p2p-all2all", driver.code, "cu")
  }
}

