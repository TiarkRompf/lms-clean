package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensor2MPI_NCCL extends Transformer {

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  import CUDATypeLess._
  // import CUBLASTypeLess._

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  // helper function for declaring a CPU array
  def cpu_array(size: Int, m: Manifest[_]): ARRAY = ARRAY(size, m)
  // specific helper function for randomly initialize a CPU array
  def cpu_random_array(size: Int, m: Manifest[_]): ARRAY = {
    val array = cpu_array(size, m)
    for (i <- (0 until size): Rep[Range]) {
      array(i) = random_value(m)
    }
    array
  }

  // helper function for declaring a GPU array
  def gpu_array(size: Int, m: Manifest[_], device: Int)(implicit __pos: SourceContext): ARRAY = {
    // FIXME(feiw) this surely doesn't compile yet
    NCCL_CHECK(CudaSetDeviceID(device))
    CUDA_MALLOC(size, m)
  }
  // helper function for declaring a GPU array with random initialization
  def gpu_random_array(size: Int, m: Manifest[_], device: Int)(implicit __pos: SourceContext): ARRAY = {
    val cpuArray = cpu_random_array(size, m)
    val gpuArray = gpu_array(size, m, device)
    CUDA_MEMCOPY(gpuArray, cpuArray, size, HOST2DEVICE, m)
    gpuArray
  }
  // helper function for declaring a GPU array with fixed value
  def gpu_fixed_array(size: Int, device: Int, value: NUM)(implicit __pos: SourceContext): ARRAY = {
    val array = gpu_array(size, value.t, device)
    val fill_fun = CUDA_FILL_FUN(value.t)
    fill_fun(array, value.x, size, DIM3(gridSize), DIM3(blockSize))
    array
  }
  // helper function for computing element-wise multiplication in GPUs
  def gpu_mult_array(size: Int, m: Manifest[_], device: Int, left_operand: Backend.Sym, right_operand: Backend.Sym)(implicit __pos: SourceContext): ARRAY = {
    def array = gpu_array(size, m, device)
    val mult_fun = CUDA_MULT_FUN(m)
    mult_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
    array
  }
  // helper function for accumulate (+=) a GPU array element-wise
  def gpu_accum_array(size: Int, m: Manifest[_], device: Int, base_operand: Backend.Sym, addition_operand: Backend.Sym)(implicit __pos: SourceContext) = {
    val accum_fun = CUDA_ACCUM_FUN(m)
    accum_fun(new ARRAY(base_operand), new ARRAY(addition_operand), size, DIM3(gridSize), DIM3(blockSize))
    Backend.Const(())
  }
  // helper function for SGD optimization on GPU array
  def gpu_sgd_array(size: Int, m: Manifest[_], device: Int, weight_operand: Backend.Sym, grad_operand: Backend.Sym, momentum_operand: Backend.Sym)(implicit __pos: SourceContext) = {
    val sgd_fun = CUDA_SGD_Nesterov_FUN(m)
    sgd_fun(new ARRAY(weight_operand), new ARRAY(grad_operand), new ARRAY(momentum_operand), size, DIM3(gridSize), DIM3(blockSize))
    Backend.Const(())
  }
  // helper function for saving GPU array
  def gpu_to_cpu_and_print(size: Int, m: Manifest[_], tensor: Backend.Sym)(implicit __pos: SourceContext):

  // lazy local functions that initialize the MPI and NCCL
  lazy val (myNCCLSize, myNCCLRank, myNCCLComm, myNCCLStream) = {
    var size = 0
    var rank = 0
    MPI_CHECK(mpi_init())
    MPI_CHECK(mpi_comm_rank(mpi_comm_world, rank))
    MPI_CHECK(mpi_comm_size(mpi_comm_world, size))
    MPI_CHECK(mpi_barrier(mpi_comm_world))

    // lazy local functions that initialize NCCL
    cudaCall(cudaSetDevice(rank))

    val commId = ncclUniqueId
    ncclCheck(ncclGetUniqueId(commId))
    MPI_CHECK(mpi_bcast_one(commId, ncclUniqueIdBytes, mpi_char, 0, mpi_comm_world))

    val comm = ncclComm
    ncclCheck(ncclCommInitRank(comm, mpiSize, commId, mpiRank))
    val stream = cudaStream
    cudaCall(cudaStreamCreateWithFlags(stream, cudaStreamNonBlocking))

    (size, rank, ncclComm, stream)
  }

  // FIXME(feiw) who calls this `finalize` to terminate the MPI and NCCL?
  lazy val finalize = {
    MPI_CHECK(mpi_finalize())
    ncclCheck(ncclCommDestroy(myNCCLComm))
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new Tensor(s, useOldMetadata = true)

      implicit val sc_: SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shape_size)

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
        case NAnno => if (myNCCLRank == 0) gpu_random_array(count, m, 0)
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_random_array(count2, m, myNCCLRank)
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val array = gpu_random_array(count, m, myNCCLRank)
          NCCL_ALLREDUCE(m, array, array, count * size_of(m), NCCL_SUM, myNCCLComm, myNCCLStream)
          array
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_weight")
      }

    // FIXME(feiw) input should be P2P communications (such as from CPU to GPU)
    // for now we just try randomization
    case Node(s, "tensor_input", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new Tensor(s, useOldMetadata = true)

      implicit val sc_: SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shape_size)

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
        case NAnno => if (myNCCLRank == 0) gpu_random_array(count, m, 0)
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_random_array(count2, m, myNCCLRank)
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val array = gpu_random_array(count, m, myNCCLRank)
          NCCL_ALLREDUCE(m, array, array, count * size_of(m), NCCL_SUM, myNCCLComm, myNCCLStream)
          array
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_weight")
      }

    case Node(s, "tensor_zeros", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new Tensor(s, useOldMetadata = true)

      implicit val sc_: SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shape_size)

      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_fixed_array(count, m, 0)
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, device.size))
          gpu_fixed_array(count2, m, myNCCLRank, NUM(Backend.Const(0), m))
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          gpu_fixed_array(count, m, myNCCLRank, NUM(Backend.Const(0), m))
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_zeros")
      }

    case Node(s, "tensor_ones", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new Tensor(s, useOldMetadata = true)

      implicit val sc_: SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val count = numeral(sourceTensor.shape_size)

      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_fixed_array(count, m, 1)
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_fixed_array(count2, m, myNCCLRank, NUM(Backend.Const(1), m))
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          gpu_fixed_array(count, m, myNCCLRank, NUM(Backend.Const(1), m))
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_ones")
      }

    case Node(s, "tensor_mult", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _) =>
      val sourceTensor = new Tensor(s, useOldMetadata = true)

      implicit val sc_: SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      // then we should run this mult op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in mult op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_mult_array(count2, m, myNCCLRank, left_operand, right_operand)
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
      }

    case Node(s, "accum_tensor", Backend.Const(anno:Anno)::(base:Backend.Exp)::(addition:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val tt = (new TENSOR(base, useOldMetadata = true)).tensor_type

      // Load the `base` and `addition`, and maybe add communication ops to resolve split annotation conflicts
      val base_operand = get_operand(base, anno, assertSame=true)
      val addition_operand = get_operand(addition, anno)
      // then we should run this accumulation op in all devices in the `anno`
      // FIXME(feiw) for now, let's assum that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in accum op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_accum_array(count2, m, myNCCLRank, base_operand, addition_operand)
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(tt.shape_size)
          gpu_accum_array(count, m, myNCCLRank, base_operand, addition_operand)
      }

    case Node(s, "optimize_tensor", Backend.Const(anno:Anno)::(weight:Backend.Exp)::(grad:Backend.Exp)::(momentum:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val tt = (new TENSOR(weight, useOldMetadata = true)).tensor_type

      // Load the `weight` `grad` `momentum`. Only `grad` might need extra communication ops to resolve split annotation conflicts
      val weight_operand = get_operand(weight, anno, assertSame=true)
      val grad_operand = get_operand(grad, anno)
      val momentum_operand = get_operand(momentum, anno, assertSame=true)
      // then we should run this optimization op in all devices in the `anno`
      // FIXME(feiw) for now, let's assum that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in optimize_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_sgd_array(count2, m, myNCCLRank, weight_operand, grad_operand, momentum_operand)
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(tt.shape_size)
          gpu_sgd_array(count, m, myNCCLRank, weight_operand, grad_operand, momentum_operand)
      }

    case Node(s, "save", (tensor:Backend.Exp)::_, _) =>
      val sourceTensor = new Tensor(s, useOldMetadata = true)

      implicit val sc_: SourceContext = sourceTensor.pos
      val m = sourceTensor.et
      val tt = sourceTensor.tensor_type
      val anno = sourceTensor.annotation

      // here we need to communicate the GPU `tensor` to CPU
      // FIXME(fei8) should we just view CPU and CPU memory as one unit (CPU0 only and one CPU memory)?
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in save_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          // collect the tensors from all GPUs in `anno` and concat them as the final result
          ???
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // copy the tensor from GPU(0) is enough
          if (myNCCLRank == 0) {
            val count = numeral(tt.shape_size)
            gpu_to_cpu_and_print(count, m, transform(tensor))
          }
      }

  }

  def get_operand(operand: Backend.Sym, anno: Anno, assertSame=false) = {
    val operand_tensor = new Tensor(operand, useOldMetadata = true)
    val operand_anno = operand_tensor.annotation
    if (operand_anno == anno) {
      transform(operand)
    } else if (assertSame) {
      throw new Exception(s"Assert that the tensor has the same annotation but it does not");
    } else {
      throw new Exception(s"TODO: not yet handling split annotation conflict $operand_tensor")
    }
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
