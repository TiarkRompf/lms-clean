package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.HashMap

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps, CUDNNTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensor2MPI_NCCLBase extends Transformer with MPIOps with CudaOps with NCCLOps with CUDNNOps {
  override val name = "DistributeTensor2MPI_NCCL"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import RangeTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  import CUDATypeLess._
  import RandomDataTypeLess._
  import NCCLTypeLess._
  import SIZE_TTypeLess._
  import CUBLASTypeLess._
  import CUDNNTypeLess._

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  // helper function for declaring a CPU array
  def cpu_array(size: Int, m: Manifest[_])(implicit pos: SourceContext): ARRAY = ARRAY(size, m)
  // specific helper function for randomly initialize a CPU array
  def cpu_random_array(size: Int, m: Manifest[_])(implicit pos: SourceContext): ARRAY = {
    val array = cpu_array(size, m)
    for (i <- RANGE_UNTIL(0, size)) {
      array(i) = random_value(m)
    }
    array
  }
  // helper function for declaring a GPU array
  def gpu_array(size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC(size, m)
  }

  // TODO: change it to better name, and add INT counterparts to other gpu functions as well
  def gpu_array1(size: INT, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC(size, m)
  }

  // TODO: change it to better name, and add INT counterparts to other gpu functions as well
  def gpu_array1_by_byte(size: INT, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC_BYTES(size, m)
  }
  // helper function for declaring a GPU array with random initialization
  def gpu_random_array(size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY =
    withComment(s"initializing random GPU array of size $size and type $m at device (pre-rename) ${device.x}") {
      val cpuArray = cpu_random_array(size, m)
      val gpuArray = gpu_array(size, m, device)
      CUDA_MEMCPY(gpuArray, cpuArray, size, HOST2DEVICE, m)
      gpuArray
    }
  // helper function for declaring a GPU array with fixed value
  val CUDA_FILL_KERNEL_MAP = scala.collection.mutable.HashMap[Manifest[_], (TOP, TOP, TOP, DIM3, DIM3) => UNIT]()
  def CUDA_FILL_FUN(m: Manifest[_])(implicit __pos: SourceContext) = CUDA_FILL_KERNEL_MAP.getOrElseUpdate(m, CUDA_FILL_KERNEL(m))
  def gpu_fixed_array(size: Int, device: INT, value: NUM)(implicit __pos: SourceContext): ARRAY =
    withComment(s"initializing fixed GPU array of size $size and type ${value.t} and device (pre-rename) ${device.x}") {
      val array = gpu_array(size, value.t, device)
      val fill_fun = CUDA_FILL_FUN(value.t)
      fill_fun(array, value, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for saving GPU array
  def gpu_to_cpu_and_print(size: Int, m: Manifest[_], tensor: Backend.Exp)(implicit __pos: SourceContext) =
    withComment(s"copying GPU array $tensor to CPU and print for size $size and type $m") {
      // declare array space in CPU
      val cpu_array = ARRAY(size, m)
      // copy the array from CPU to GPU
      CUDA_MEMCPY(cpu_array, new ARRAY(tensor), size, DEVICE2HOST, m)
      cpu_array.print
    }

  // lazy local functions that initialize the MPI and NCCL
  lazy val (myNCCLSizeRep, myNCCLRankRep, myNCCLCommRep, myNCCLStreamRep) = withComment("setting up the MPI/NCCL environment") {

    val size = var_new(unit(0))
    val rank = var_new(unit(0))
    MPI_CHECK(mpi_init())
    MPI_CHECK(mpi_comm_rank(mpi_comm_world, rank))
    MPI_CHECK(mpi_comm_size(mpi_comm_world, size))
    MPI_CHECK(mpi_barrier(mpi_comm_world))

    cudaCall(cudaSetDevice(readVar(rank)))

    val commId = ncclUniqueId
    ncclCheck(ncclGetUniqueId(commId))
    MPI_CHECK(mpi_bcast_one(commId, ncclUniqueIdBytes, mpi_char, unit(0), mpi_comm_world))

    val comm = ncclComm
    ncclCheck(ncclCommInitRank(comm, readVar(size), commId, readVar(rank)))
    val stream = cudaStream
    cudaCall(cudaStreamCreateWithFlags(stream, cudaStreamNonBlocking))

    (readVar(size), readVar(rank), comm, stream)
  }

  def myNCCLSize(implicit __pos: SourceContext) = INT(Unwrap(myNCCLSizeRep))
  def myNCCLRank(implicit __pos: SourceContext) = INT(Unwrap(myNCCLRankRep))
  def myNCCLComm(implicit __pos: SourceContext) = TOP(Unwrap(myNCCLCommRep), manifest[ncclCommT])
  def myNCCLStream(implicit __pos: SourceContext) = TOP(Unwrap(myNCCLStreamRep), manifest[cudaStreamT])

  def set_up_mpi_nccl(implicit __pos: SourceContext) = { val dummy = myNCCLSize }
  def finalize_mpi_nccl(implicit __pos: SourceContext) = {
    MPI_CHECK(mpi_finalize())
    ncclCheck(ncclCommDestroy(myNCCLCommRep))
  }

  // lazy local function that initializes CUDNN
  lazy val (myCUDNNCommRep) = withComment("setting up the CUDNN environment") {
    val cudnn = cudnnHandle
    cudnnCheck(cudnnCreate(cudnn))
    cudnn
  }

  def myCUDNNComm(implicit __pos: SourceContext) = TOP(Unwrap(myCUDNNCommRep), manifest[cudnnHandleT])

  // var cudnnTensor2Desc: HashMap[Backend.Sym, (TOP, String)] = HashMap()
  var cudnnTensor2Desc: HashMap[Seq[Int], (TOP, String)] = HashMap()
  var cudnnConv2Desc: HashMap[Seq[Int], CUDNN_CONV_DESCRIPTOR] = HashMap()
  def set_up_cudnn(implicit __pos: SourceContext) = {
    val dummy = myCUDNNComm
    // cudnnTensor2Desc = HashMap()  // todo: FIXME
  }
  def finalize_cudnn(implicit __pos: SourceContext) = {
    cudnnCheck(cudnnDestroy(myCUDNNCommRep))
    cudnnTensor2Desc foreach {
      case (_, (desc, "tensor")) => CUDNN_DESTROY_TENSOR_DESCRIPTOR(desc)
      case (_, (desc, "filter")) => CUDNN_DESTROY_FILTER_DESCRIPTOR(desc)
      case _ => throw new Exception("Unknown kind of cudnn tensor descriptor")
    }
    cudnnConv2Desc foreach {
      case (_, desc) => CUDNN_DESTROY_CONV_DESCRIPTOR(desc)
    }
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shapeSize)

      // allocate memory on the give device (see annotation)
      // 1. if the annotation is splitting on a dimension that is not in weight,
      //    we need to duplicate the weights across the devices in the annotation.
      //    we can do so by randomly initialize the array in CPU, then memcpy them to GPU
      //    then AllReduce the random initialization by NCCL.
      // 2. if the annotation is splitting on a dimension that is in weight, we need
      //    to split the weights across the devices in the annotation. We can do so by
      //    randomly initialize the smaller arrays in CPU, then memcpy them to GPU.
      // 3. if the annotation is NAnno, then we initialize the tensor only on GPU(0)
      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_random_array(count, m, 0).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_random_array(count2, m, myNCCLRank).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val array = gpu_random_array(count, m, myNCCLRank)
          NCCL_CHECK(NCCL_ALLREDUCE(m, array, array, INT(count) * SIZE_OF(m), NCCL_SUM, myNCCLComm, myNCCLStream))
          array.x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_weight")
      }

    // FIXME(feiw) input should be P2P communications (such as from CPU to GPU)
    // for now we just try randomization
    case Node(s, "tensor_input", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shapeSize)

      // allocate memory on the give device (see annotation)
      // 1. if the annotation is splitting on a dimension that is not in weight,
      //    we need to duplicate the weights across the devices in the annotation.
      //    we can do so by randomly initialize the array in CPU, then memcpy them to GPU
      //    then AllReduce the random initialization by NCCL.
      // 2. if the annotation is splitting on a dimension that is in weight, we need
      //    to split the weights across the devices in the annotation. We can do so by
      //    randomly initialize the smaller arrays in CPU, then memcpy them to GPU.
      // 3. if the annotation is NAnno, then we initialize the tensor only on GPU(0)
      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_random_array(count, m, 0).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_random_array(count2, m, myNCCLRank).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val array = gpu_random_array(count, m, myNCCLRank)
          NCCL_CHECK(NCCL_ALLREDUCE(m, array, array, INT(count) * SIZE_OF(m), NCCL_SUM, myNCCLComm, myNCCLStream))
          array.x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_weight")
      }

    case Node(s, "tensor_zeros", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shapeSize)

      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_fixed_array(count, 0, 0).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_fixed_array(count2, myNCCLRank, NUM(Backend.Const(0), m)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          gpu_fixed_array(count, myNCCLRank, NUM(Backend.Const(0), m)).x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_zeros")
      }

    case Node(s, "tensor_ones", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shapeSize)

      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_fixed_array(count, 0, 1).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_fixed_array(count2, myNCCLRank, NUM(Backend.Const(1), m)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          gpu_fixed_array(count, myNCCLRank, NUM(Backend.Const(1), m)).x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_ones")
      }

    case Node(s, "save", (tensor:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val sourceTensor = new TENSOR(tensor, useOldMetadata = true)
      val m = sourceTensor.et
      val tt = sourceTensor.resultType
      val anno = sourceTensor.annotation

      // here we need to communicate the GPU `tensor` to CPU
      // FIXME(feiw) should we just view CPU and CPU memory as one unit (CPU0 only and one CPU memory)?
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in save_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          // collect the tensors from all GPUs in `anno` and concat them as the final result

          val root = 0
          val count = numeral(sourceTensor.shapeSize)
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))

          // declare recv buffer
          generate_comment("Only declare recv buffer if this is the root")
          val recvbuf = ARRAY(IF (EQUAL(myNCCLRank, INT(root))) { CUDA_MALLOC(count, m) } { CUDA_MALLOC(0, m) })

          // Gather + Concat + Print
          generate_comment("Gather by groups of NCCL send/recv")
          NCCL_CHECK(NCCL_GROUP_START)
          NCCL_CHECK(NCCL_SEND(m, new ARRAY(transform(tensor)), SIZE_T(count), root, myNCCLComm, myNCCLStream))
          IF (EQUAL(myNCCLRank, INT(root))) {
            for (r <- RANGE_UNTIL(0, myNCCLSize)) {
              NCCL_CHECK(NCCL_RECV(m, recvbuf.slice(INT(r * count2), INT((r + 1) * count2)), SIZE_T(count), r, myNCCLComm, myNCCLStream))
            }
          } { UNIT(Backend.Const(())) }
          NCCL_CHECK(NCCL_GROUP_END)

          generate_comment("print the array only if this is the root")
          IF (EQUAL(myNCCLRank, INT(root))) {
            gpu_to_cpu_and_print(count, m, recvbuf.x); UNIT(Backend.Const(()))
          } { UNIT(Backend.Const(())) }

          Backend.Const(())

        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // copy the tensor from GPU(0) is enough
          IF (EQUAL(myNCCLRank, INT(0))) {
            val count = numeral(sourceTensor.shapeSize)
            gpu_to_cpu_and_print(count, m, transform(tensor))
            UNIT(Backend.Const(()))
          } { UNIT(Backend.Const(())) }
          Backend.Const(())
      }


    case Node(s, "tensor_result", tt::anno::(op:Backend.Sym)::Backend.Const(i:Int)::_, _) => // subst(s)
      Adapter.g.globalDefsCache.get(transform(op).asInstanceOf[Backend.Sym]) match {
        case Some(Node(_, "tuple-view", xs: List[Backend.Sym], _)) => xs(i)
        case a => throw new Exception(s"$a is not a tuple view")
      }

    case Node(s, op, _, _) if op.startsWith("tensor_") || op.startsWith("tensors_") =>
      throw new Exception(s"not yet handling $n in distributedTensor2MPINCCL transformation")

    case _ => super.transform(n)
  }

  def tensor_shape(tensor: Backend.Exp, useOldMetadata: Boolean = false): Seq[Int] =
    (new TENSOR(tensor, useOldMetadata)).shapeSize

  def get_operand(operand: Backend.Exp, anno: Anno, assertSame: Boolean = false) = {
    val operand_tensor = new TENSOR(operand, useOldMetadata = true)
    val operand_anno = operand_tensor.annotation
    if (operand_anno == anno) {
      transform(operand)
    } else if (assertSame) {
      throw new Exception(s"Assert that the tensor has the same annotation but it does not: ${operand_anno} v.s. ${anno}");
    } else {
      throw new Exception(s"TODO: not yet handling split annotation conflict $operand_tensor")
    }
  }

  override def wrapGraph(graph: Graph): Unit = {
    // analyze the graph to see if we need to
    // 0. Set up MPI/NCCL (this is always a yes)
    // 1. Set up Cublas handler
    // 2. Set up Cudnn handler
    val analysis = new DistributeTensor2MPI_NCCLAnalysis
    analysis.apply(graph)

    // FIXME(feiw) dummy __pos
    implicit val pos: SourceContext = Adapter.oldSourceMap.head._2

    // pre set-ups
    // 0. set up MPI/NCCL
    set_up_mpi_nccl
    // 1. maybe set up cublas
    if (analysis.hasCublas) set_up_cublas
    // 2. maybe set up cudnn
    if (analysis.hasCudnn) set_up_cudnn

    super.apply(graph)

    // post clean-ups
    // 1. maybe clean up cublas
    if (analysis.hasCublas) finalize_cublas
    // 2. maybe clean up cudnn
    if (analysis.hasCudnn) finalize_cudnn
    // 0. clean up mpi/nccl
    finalize_mpi_nccl
  }

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g

    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
