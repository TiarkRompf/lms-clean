package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
import lms.collection.mutable.ArrayTypeLess
import lms.thirdparty.array_computation.{CUDATypeLess, CCodeGenCudaOps, CudaOps}


class NCCLTest extends TutorialFunSuite {
  val under = "thirdparty/nccl/"

  abstract class DslDriverCNCCL[A: Manifest, B: Manifest] extends DslDriverC[A,B] with NCCLOps with CudaOps with SizeTOps with MPIOps { q =>
    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCudaOps with CCodeGenSizeTOps with CCodeGenMPI {
      val IR: q.type = q

      registerHeader("<nccl_header.h>")
      registerHeader("<nccl.h>")
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
        // the same as cudaStreamCreate(s)
        cudaStreamCreateWithFlags(s, cudaStreamDefault)
        var sendbuff = cudaMalloc2[Float](0)
        var recvbuff = cudaMalloc2[Float](0)
        ncclCheck(ncclAllReduce(sendbuff, recvbuff, SizeT(0), ncclFloat, ncclSum, comms, s))
        cudaCall(cudaStreamSynchronize(s))
        printf("end")
      }
    }
    // System.out.println(indent(driver.code))
  }

  test("single-process-single-device") {
    // https://docs.nvidia.com/deeplearning/nccl/user-guide/docs/examples.html
    // example 1, but single device
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
        cudaCall(cudaMemset[Float](sendbuff, 1, SizeT(size)))
        cudaCall(cudaMemset[Float](recvbuff, 0, SizeT(size)))
        cudaCall(cudaStreamCreateWithFlags(s, cudaStreamDefault))

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
}

