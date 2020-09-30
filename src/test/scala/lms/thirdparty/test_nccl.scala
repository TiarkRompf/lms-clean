package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection._
import lms.collection.mutable.ArrayTypeLess
import lms.thirdparty.array_computation.{CUDATypeLess, CCodeGenCudaOps, CudaOps}


class NCCLTest extends TutorialFunSuite {
  val under = "thirdparty/"

  abstract class DslDriverCNCCL[A: Manifest, B: Manifest] extends DslDriverC[A,B] with NCCLOps with CudaOps with SizeTOps{ q =>
    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCudaOps with CCodeGenSizeTOps {
      val IR: q.type = q
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
    System.out.println(indent(driver.code))
  }

  // use MPI and NCCL together
  abstract class DslDriverCNCCLMPI[A: Manifest, B: Manifest] extends DslDriverCNCCL[A,B] with MPIOps { q =>
    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCudaOps with CCodeGenSizeTOps with CCodeGenMPI {
      val IR: q.type = q
    }
  }

  test("one-device-per-process") {
    // https://docs.nvidia.com/deeplearning/nccl/user-guide/docs/examples.html
    // example 2
    val driver = new DslDriverCNCCLMPI[Int, Unit] {

      @virtualize
      def snippet(arg:Rep[Int]) = {
        val size = 32*1024*1024
        var myRank = 0
        var nRanks = 0
        val localRank = 0

        // initializing MPI
        mpi_init()
        mpi_comm_rank(mpi_comm_world, myRank)
        mpi_comm_size(mpi_comm_world, nRanks)

        // calculating localRank
        // for now, localRank = 0

        // get NCCL unique ID at rank 0 and broadcast it to all others
        var id = ncclUniqueId
        if (myRank == 0) {
          ncclCheck(ncclGetUniqueId(id))
        }
        mpi_bcast_one[ncclUniqueId](id, ncclUniqueIdBytes, mpi_byte, 0, mpi_comm_world)

        // picking a GPU based on localRank, allocate device buffers
        cudaCall(cudaSetDevice(localRank))
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
        mpi_finalize()

        printf("[MPI Rank %d] Success \n", myRank)
      }
    }
    // System.out.println(indent(driver.code))
  }
  
  test("mpi-reduce") {
    // based on file: main/scala/lms/thirdparty/nccl/cu_examples/mpi/mpi_test.cu
    val driver = new DslDriverCNCCLMPI[Int, Unit] {
      @virtualize
      def snippet(arg:Rep[Int]) = {
        val SIZE = 128
        val NITERS = 1

        val commId = ncclUniqueId
        var size = 0
        var rank = 0

        mpi_init()
        mpi_comm_rank(mpi_comm_world, size)
        mpi_comm_size(mpi_comm_world, rank)

        if (arg < size) {
          if (rank == 0) {
            printf("Usage : %s <GPU list per rank\n", arg)
          }
          exit(1)
        }

        // change to atoi(argv[rank+1]) later
        val gpu = 0

        // We have to set our device before NCCL init
        cudaCall(cudaSetDevice(gpu))
        mpi_barrier(mpi_comm_world)

        // NCCL Communicator creation
        val comm = ncclComm
        ncclCheck(ncclGetUniqueId(commId))
        mpi_bcast_one[ncclUniqueId](commId, ncclUniqueIdBytes, mpi_char, 0, mpi_comm_world)
        ncclCheck(ncclCommInitRank(comm, size, commId, rank))

        // cuda stream creation
        val stream = cudaStream
        cudaCall(cudaStreamCreateWithFlags(stream, cudaStreamNonBlocking))

        // initialize input values
        // split dptr to dptr_send and dptr_recv to avoid pointer addition
        var dptr_send = cudaMalloc2[Int](SIZE)
        var dptr_recv = cudaMalloc2[Int](SIZE)
        var value = cudaMalloc2[Int](SIZE)
        var i = 0;
        while (i < SIZE) {
          value(i) = rank + 1
          i = i + 1
        }
        cudaCall(cudaMemcpy[Int](dptr_send, value, SizeT(SIZE), host2device))

        // compute final value
        val ref = size * (size + 1) / 2

        // run allreduce
        var errors = 0
        i = 0;
        while (i < NITERS) {
          ncclCheck(ncclAllReduce(dptr_send, dptr_recv, SizeT(SIZE), ncclInt, ncclSum, comm, stream))
          i = i + 1
        }

        // Check results
        cudaCall(cudaStreamSynchronize(stream))
        cudaCall(cudaMemcpy[Int](value, dptr_recv, SizeT(SIZE), host2device))
        i = 0
        while (i < SIZE) {
          if (value(i) != ref) {
            errors = errors + 1
            printf("error")
          }
          i = i + 1
        }
        cudaCall(cudaFree(dptr_recv))
        cudaCall(cudaFree(dptr_send))

        val errors1 = errors
        mpi_allReduce_one_inplace[Int](errors1, 1, mpi_integer, mpi_sum, mpi_comm_world)
        if (rank == 0) {
          if (errors1 > 0) {
            printf("%d errors. Test FAILED.\n", errors1)
          } else {
            printf("test PASSED.\n")
          }
        }
        mpi_finalize();
        ncclCheck(ncclCommDestroy(comm))

        printf("success\n")
      }
    }
    // System.out.println(indent(driver.code))
  }
}

