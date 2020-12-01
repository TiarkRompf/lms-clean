package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensor2MPI_NCCL extends Transformer with MPIOps with CudaOps with NCCLOps {

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
  // helper function for declaring a GPU array with random initialization
  def gpu_random_array(size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY =
    withComment(s"initializing random GPU array of size $size and type $m at device (pre-rename) ${device.x}") {
      val cpuArray = cpu_random_array(size, m)
      val gpuArray = gpu_array(size, m, device)
      CUDA_MEMCPY(gpuArray, cpuArray, size, HOST2DEVICE, m)
      gpuArray
    }
  // helper function for declaring a GPU array with fixed value
  def gpu_fixed_array(size: Int, device: INT, value: NUM)(implicit __pos: SourceContext): ARRAY =
    withComment(s"initializing fixed GPU array of size $size and type ${value.t} and device (pre-rename) ${device.x}") {
      val array = gpu_array(size, value.t, device)
      val fill_fun = CUDA_FILL_FUN(value.t)
      fill_fun(array, value, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for computing element-wise multiplication in GPUs
  def gpu_mult_array(size: Int, m: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing MULT on GPU for size $size and type $m at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      val array = gpu_array(size, m, device)
      val mult_fun = CUDA_MULT_FUN(m)
      mult_fun(new ARRAY(left_operand), new ARRAY(right_operand), array, size, DIM3(gridSize), DIM3(blockSize))
      array
    }
  // helper function for computing dot product in GPUs
  // the dot is for 2 matrices of shape (m x k) and (k x n)
  def gpu_dot_array(size: Int, man: Manifest[_], device: INT, left_operand: Backend.Exp, right_operand: Backend.Exp, m: Int, n: Int, k: Int)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing DOT on GPU for size $size and type $man at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand") {
      assert(man == manifest[Float], s"Using cublas for dot operation, which only support float element type, but got: ${man}")
      val array = gpu_array(size, man, device)
      // we use transpose option because the cublas function assume column-major
      CUBLAS_SGEMM(CUBLAS_HANDLE, CUBLAS_OP_T, CUBLAS_OP_T, m, n, k, ONE, new ARRAY(left_operand), m, new ARRAY(right_operand), k, ZERO, array, m)
      array
    }
  // helper function for computing dot product in GPUs
  // the dot is for 2 matrices of shape (m x k) and (k x n), allowing left/right operand to be transposed
  def gpu_dot_array_with_transpose(size: Int, man: Manifest[_], device: INT, left_operand: Backend.Exp, transL: Boolean, right_operand: Backend.Exp, transR: Boolean, m: Int, n: Int, k: Int)(implicit __pos: SourceContext): ARRAY =
    withComment(s"computing DOT on GPU for size $size and type $man at device (pre-rename) ${device.x} with left_operand $left_operand and right_operand $right_operand with transpose options") {
      assert(man == manifest[Float], s"Using cublas for dot operation, which only support float element type, but got: ${man}")
      val array = gpu_array(size, man, device)
      // the transpose options should also take account into the column-major assumption of the cublas function
      CUBLAS_SGEMM(CUBLAS_HANDLE, if (transL) CUBLAS_OP_N else CUBLAS_OP_T, if (transR) CUBLAS_OP_N else CUBLAS_OP_T,
        m, n, k, ONE, new ARRAY(left_operand), m, new ARRAY(right_operand), k, ZERO, array, m)
      array
    }
  // helper function for accumulate (+=) a GPU array element-wise
  def gpu_accum_array(size: Int, m: Manifest[_], device: INT, base_operand: Backend.Exp, addition_operand: Backend.Exp)(implicit __pos: SourceContext) =
    withComment(s"computing ACCUM on GPU for size $size and type $m at device (pre-rename) ${device.x} with base_operand $base_operand and addition_operand $addition_operand") {
      CUDA_SET_DEVICE(device)
      val accum_fun = CUDA_ACCUM_FUN(m)
      accum_fun(new ARRAY(base_operand), new ARRAY(addition_operand), size, DIM3(gridSize), DIM3(blockSize))
      Backend.Const(())
    }
  // helper function for SGD optimization on GPU array
  def gpu_sgd_array(size: Int, m: Manifest[_], device: INT, weight_operand: Backend.Exp, grad_operand: Backend.Exp, momentum_operand: Backend.Exp)(implicit __pos: SourceContext) =
    withComment(s"computing SGD on GPU for size $size and type $m at device (pre-name) ${device.x} with weight $weight_operand, grad $grad_operand, and momentum $momentum_operand") {
      CUDA_SET_DEVICE(device)
      val sgd_fun = CUDA_SGD_Nesterov_FUN(m)
      sgd_fun(new ARRAY(weight_operand), new ARRAY(grad_operand), new ARRAY(momentum_operand), size, DIM3(gridSize), DIM3(blockSize))
      Backend.Const(())
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

    // lazy local functions that initialize NCCL
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

  // FIXME(feiw) who calls this `finalize` to terminate the MPI and NCCL?
  lazy val finalize_nccl = {
    MPI_CHECK(mpi_finalize())
    ncclCheck(ncclCommDestroy(myNCCLCommRep))
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
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
      val count = numeral(sourceTensor.shape_size)

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
      val count = numeral(sourceTensor.shape_size)

      anno match {
        case NAnno => if (myNCCLRank == 0) gpu_fixed_array(count, 0, 1).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_fixed_array(count2, myNCCLRank, NUM(Backend.Const(1), m)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          gpu_fixed_array(count, myNCCLRank, NUM(Backend.Const(1), m)).x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_ones")
      }

    case Node(s, "tensor_dot", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // get the shape of `left` and `right` tensor
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)
      assert(left_shape.size == 2 && right_shape.size == 2 && left_shape(1) == right_shape(0),
          s"shapes of tensor_dot are invalid left operand shape: $left_shape x right operand shape: $right_shape")

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      // then we should run this dot op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_dot_array(count2, m, myNCCLRank, left_operand, right_operand, left_shape(0), right_shape(1), left_shape(1)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(tt.shapeSize)
          val dot_res = gpu_dot_array(count, m, myNCCLRank, left_operand, right_operand, left_shape(0), right_shape(1), left_shape(1))
          NCCL_ALLREDUCE(m, dot_res, dot_res, SIZE_T(count), NCCL_SUM, myNCCLComm, myNCCLStream)
          dot_res.x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_dot")
      }

    case Node(s, "tensor_dot_with_transpose", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(transL:Boolean)::Backend.Const(transR:Boolean)::(left:Backend.Sym)::(right:Backend.Sym)::_,_) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
      val m = sourceTensor.et

      // get the shape of `left` and `right` tensor
      val left_shape = tensor_shape(left, useOldMetadata = true)
      val right_shape = tensor_shape(right, useOldMetadata = true)
      assert(left_shape.size == 2 && right_shape.size == 2, s"For now, only handles matrix matrix dot")

      // Load the `left` and `right`, and maybe add communication ops to resolve split annotation conflicts
      val left_operand = get_operand(left, anno)
      val right_operand = get_operand(right, anno)
      // then we should run this dot op in all devices in the `anno`
      // FIXME(feiw) for now, let's assume that `anno` is for all devices
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in dot op")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))
          gpu_dot_array_with_transpose(count2, m, myNCCLRank, left_operand, transL, right_operand, transR,
            if (transL) left_shape(1) else left_shape(0), if (transR) right_shape(0) else right_shape(1), if (transL) left_shape(0) else left_shape(1)).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(tt.shapeSize)
          val dot_res = gpu_dot_array_with_transpose(count, m, myNCCLRank, left_operand, transL, right_operand, transR,
            if (transL) left_shape(1) else left_shape(0), if (transR) right_shape(0) else right_shape(1), if (transL) left_shape(0) else left_shape(1))
          NCCL_ALLREDUCE(m, dot_res, dot_res, SIZE_T(count), NCCL_SUM, myNCCLComm, myNCCLStream)
          dot_res.x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_dot_with_transpose")
      }

    case Node(s, "tensor_mult", Backend.Const(tt: TensorType)::Backend.Const(anno:Anno)::(left:Backend.Sym)::(right:Backend.Sym)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)

      implicit val sc_ : SourceContext = sourceTensor.pos
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
          gpu_mult_array(count2, m, myNCCLRank, left_operand, right_operand).x
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          throw new Exception(s"TODO: not yet handling SAnno with AllReduce")
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in tensor_mult")
      }

    case Node(s, "accum_tensor", Backend.Const(anno:Anno)::(base:Backend.Exp)::(addition:Backend.Exp)::_, _) =>
      val sourceTensor = new TENSOR(base, useOldMetadata = true)

      implicit val pos = Adapter.oldSourceMap(s)
      val tt = sourceTensor.tensor_type
      val m = sourceTensor.et

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
          Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(sourceTensor.shape_size)
          gpu_accum_array(count, m, myNCCLRank, base_operand, addition_operand)
          Backend.Const(())
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in accum_tensor")
      }

    case Node(s, "optimize_tensor", Backend.Const(anno:Anno)::(weight:Backend.Exp)::(grad:Backend.Exp)::(momentum:Backend.Exp)::_, _) =>
      val sourceTensor = new TENSOR(weight, useOldMetadata = true)

      implicit val pos = Adapter.oldSourceMap(s)
      val tt = sourceTensor.tensor_type
      val m = sourceTensor.et
      val anno = sourceTensor.annotation

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
          Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          val count = numeral(sourceTensor.shape_size)
          gpu_sgd_array(count, m, myNCCLRank, weight_operand, grad_operand, momentum_operand)
          Backend.Const(())
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in optimize_tensor")
      }

    case Node(s, "save", (tensor:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val sourceTensor = new TENSOR(tensor, useOldMetadata = true)
      val m = sourceTensor.et
      val tt = sourceTensor.tensor_type
      val anno = sourceTensor.annotation

      // here we need to communicate the GPU `tensor` to CPU
      // FIXME(feiw) should we just view CPU and CPU memory as one unit (CPU0 only and one CPU memory)?
      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in save_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) if tt.contains(dim) =>
          // collect the tensors from all GPUs in `anno` and concat them as the final result

          val root = 0
          val count = numeral(sourceTensor.shape_size)
          val count2 = numeral(tt.shapeSizeAfterSplit(dim, devices.size))

          // declare recv buffer
          generate_comment("Only declare recv buffer if this is the root")
          val recvbuf = ARRAY(IF (EQUAL(myNCCLRank, INT(root))) { CUDA_MALLOC(count, m) } { CUDA_MALLOC(0, m) })

          // Gather + Concat + Print
          generate_comment("Gather by groups of NCCL send/recv")
          NCCL_CHECK(NCCL_GROUP_START)
          NCCL_CHECK(NCCL_SEND(m, new ARRAY(tensor), SIZE_T(count), root, myNCCLComm, myNCCLStream))
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
            val count = numeral(sourceTensor.shape_size)
            gpu_to_cpu_and_print(count, m, transform(tensor))
            UNIT(Backend.Const(()))
          } { UNIT(Backend.Const(())) }
          Backend.Const(())
      }

    case _ => super.transform(n)
  }

  def tensor_shape(tensor: Backend.Exp, useOldMetadata: Boolean = false): Seq[Int] =
    (new TENSOR(tensor, useOldMetadata)).shape_size

  def get_operand(operand: Backend.Exp, anno: Anno, assertSame: Boolean = false) = {
    val operand_tensor = new TENSOR(operand, useOldMetadata = true)
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
