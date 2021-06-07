package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.HashMap
import scala.collection.immutable.Set

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps, CUDNNTypeLess, CLibTypeLess}
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
  import CLibTypeLess._

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

  def GPU_ARRAY(size: INT, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
    CUDA_SET_DEVICE(device)
    CUDA_MALLOC(size, m)
  }

  def GPU_ARRAY_BY_BYTE(size: INT, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY = {
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

  def ScanFileRank(fileName: Rep[String], rank: INT, array: ARRAY, count: Int)(implicit pos: SourceContext) = {
    val function = array.et match {
      case m if m == manifest[Float] => "scan_float_rank"
      case m if m == manifest[Int] => "scan_int_rank"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, Unwrap(fileName), rank.x, array.x, lms.core.Backend.Const(count))(Seq[Int](), Seq[Int](1,2,3), Set[Int]())
  }

  def ScanFile(scan: ARRAY, count: INT, filenameFormat: Rep[String], filenameArgs: Rep[Any]*)(implicit pos: SourceContext) = {
    val function = scan.et match {
      case m if m == manifest[Float] => "scan_float_array"
      case m if m == manifest[Int] => "scan_int_array"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, scan.x::count.x::Unwrap(filenameFormat)::filenameArgs.map(Unwrap).toList:_*)(Seq[Int](), Seq[Int](0,1), Set[Int]())
  }

  def CheckFileRank(fileName: Rep[String], rank: INT, check: ARRAY, count: Int)(implicit pos: SourceContext) = {
    val function = check.et match {
      case m if m == manifest[Float] => "check_float_array_rank"
      case m if m == manifest[Int] => "check_int_array_rank"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, Unwrap(fileName), rank.x, check.x, lms.core.Backend.Const(count))(Seq[Int](1, 2), Seq[Int](), Set[Int](), Adapter.CTRL)
  }

  def CheckFile(filename: Rep[String], check: ARRAY, count: Int)(implicit pos: SourceContext) = {
    val function = check.et match {
      case m if m == manifest[Float] => "check_float_array"
      case m if m == manifest[Int] => "check_int_array"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, Unwrap(filename), check.x, Backend.Const(count))(Seq[Int](1), Seq[Int](), Set[Int](), Adapter.CTRL)
  }

  def CheckFile(check: ARRAY, count: Int, filenameFormat: Rep[String], filenameArgs: Rep[Any]*)(implicit pos: SourceContext) = {
    val function = check.et match {
      case m if m == manifest[Float] => "check_float_array"
      case m if m == manifest[Int] => "check_int_array"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, (check.x)::Backend.Const(count)::Unwrap(filenameFormat)::filenameArgs.map(Unwrap).toList:_*)(Seq[Int](0), Seq[Int](), Set[Int](), Adapter.CTRL)
  }

  // helper function for initializing a GPU array from binary file
  def gpu_scanner_array(name: String, size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY =
    withComment(s"initializing GPU array of size $size and type $m at device (pre-rename) ${device.x} from binary file ${name}") {
      val cpuArray = cpu_array(size, m)
      val gpuArray = gpu_array(size, m, myNCCLRank)
      ScanFileRank(unit("golden/" + name), device, cpuArray, size)
      CUDA_MEMCPY(gpuArray, cpuArray, size, HOST2DEVICE, m)
      gpuArray
    }
  // helper function for initializing a GPU array
  def gpu_scanner_array(size: Int, m: Manifest[_], device: INT, filenameFormat: String, filenameArgs: TOP*)(implicit __pos: SourceContext): ARRAY = {
    withComment(s"initializing GPU array of size $size and type $m") {
      val cpuArray = cpu_array(size, m)
      val gpuArray = gpu_array(size, m, myNCCLRank)
      ScanFile(cpuArray, size, unit(filenameFormat), filenameArgs.map(arg=>Wrap[Any](arg.x)):_*)
      CUDA_MEMCPY(gpuArray, cpuArray, size, HOST2DEVICE, m)
      gpuArray
    }
  }
  // helper function for checking a GPU array against golden values
  def check_gpu_array(array: ARRAY, name: String, size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext) =
    withComment(s"checking GPU array of size $size and type $m at device (pre-name) ${device.x} again binary file ${name}") {
      val checkArray = cpu_array(size, m)
      CUDA_MEMCPY(checkArray, array, size, DEVICE2HOST, m)
      CheckFile(unit(name), checkArray, size)
      // CheckFileRank(unit("golden/" + name), device, checkArray, size)
    }
  // helper function fo checking a GPU array against golden values
  def check_gpu_array(array: ARRAY, size: Int, m: Manifest[_], device: INT, filenameFormat: String, filenameArgs: Backend.Exp*)(implicit __pos: SourceContext) =
    withComment(s"checking GPU array of size $size and type $m") {
      val checkArray = cpu_array(size, m)
      CUDA_MEMCPY(checkArray, array, size, DEVICE2HOST, m)
      CheckFile(checkArray, size, unit(filenameFormat), filenameArgs.toSeq.map(Wrap[Any](_)):_*)
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

  var hasCublas = false
  var hasCudnn = false

  def setupCublasCudnn: Unit = {
    if (hasCublas) set_up_cublas
    if (hasCudnn) set_up_cudnn
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
  var cudnnActv2Desc: HashMap[(String, Float), CUDNN_ACTIVATION_DESCRIPTOR] = HashMap()
  var cudnnPool2Desc: HashMap[(String, Seq[Int]), CUDNN_POOLING_DESCRIPTOR] = HashMap()
  def set_up_cudnn(implicit __pos: SourceContext) = {
    val dummy = myCUDNNComm
  }
  def finalize_cudnn(implicit __pos: SourceContext) = {
    cudnnCheck(cudnnDestroy(myCUDNNCommRep))
  }

  override def traverse(ns: Seq[Node], res: Block): Unit = {
    // set hashmaps for cudnn descriptors
    val savedCudnnTensor2Desc = cudnnTensor2Desc
    val savedCudnnConv2Desc = cudnnConv2Desc
    val savedCudnnActv2Desc = cudnnActv2Desc
    val savedCudnnPool2Desc = cudnnPool2Desc
    cudnnTensor2Desc = HashMap[Seq[Int], (TOP, String)]()
    cudnnConv2Desc = HashMap[Seq[Int], CUDNN_CONV_DESCRIPTOR]()
    cudnnActv2Desc = HashMap[(String, Float), CUDNN_ACTIVATION_DESCRIPTOR]()
    cudnnPool2Desc = HashMap[(String, Seq[Int]), CUDNN_POOLING_DESCRIPTOR]()

    super.traverse(ns, res)

    // clean up hashmaps for cudnn descriptors
    cudnnTensor2Desc foreach {
      case n@(_, (desc, "tensor")) => CUDNN_DESTROY_TENSOR_DESCRIPTOR(desc)
      case n@(_, (desc, "filter")) => CUDNN_DESTROY_FILTER_DESCRIPTOR(desc)
      case _ => throw new Exception("Unknown kind of cudnn tensor descriptor")
    }
    cudnnConv2Desc foreach {
      case n@(_, desc) => CUDNN_DESTROY_CONV_DESCRIPTOR(desc)
    }
    cudnnActv2Desc foreach {
      case (_, desc) => CUDNN_DESTROY_ACTIVATION_DESCRIPTOR(desc)
    }
    cudnnPool2Desc foreach {
      case (_, desc) => CUDNN_DESTROY_POOLING_DESCRIPTOR(desc)
    }
    cudnnTensor2Desc = savedCudnnTensor2Desc
    cudnnConv2Desc = savedCudnnConv2Desc
    cudnnActv2Desc = savedCudnnActv2Desc
    cudnnPool2Desc = savedCudnnPool2Desc
  }

  override def transform(n: Node): Backend.Exp = n match {
    // track the world_rank to set up MPI_NCCL
    case Node(s, "world_rank", _, _) =>
      set_up_mpi_nccl
      myNCCLRank.x

    // track the world_size to set up MPI_NCCL
    case Node(s, "world_size", _, _) =>
      set_up_mpi_nccl
      myNCCLSize.x

    // track the world_finalize to finalize the MPI_NCCL
    case Node(s, "world_finalize", _, _) =>
      finalize_mpi_nccl
      Backend.Const(())

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_scanner_array(count, sourceTensor.et, myNCCLRank, filenameFormat, filenameArgs.map(arg=>new TOP(transform(arg))):_*).x

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      tt.tensorName match {
        case Some(name) => gpu_scanner_array(name, count, sourceTensor.et, myNCCLRank).x
        case None => gpu_random_array(count, sourceTensor.et, myNCCLRank).x
      }

    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_scanner_array(count, sourceTensor.et, myNCCLRank, filenameFormat, filenameArgs.map(arg=>new TOP(transform(arg))):_*).x

    case Node(s, "tensor_input", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      tt.tensorName match {
        case Some(name) => gpu_scanner_array(name, count, sourceTensor.et, myNCCLRank).x
        case None => gpu_random_array(count, sourceTensor.et, myNCCLRank).x
      }

    case Node(s, "tensor_zeros", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_fixed_array(count, myNCCLRank, NUM(Backend.Const(0), sourceTensor.et)).x

    case Node(s, "tensor_ones", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_fixed_array(count, myNCCLRank, NUM(Backend.Const(1), sourceTensor.et)).x

    case Node(s, "save_tensor", (tensor:Backend.Exp)::_, _) =>
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

    case Node(s, "check_tensor", (tensor:Backend.Exp)::Backend.Const(name:String)::(xs:List[Backend.Exp]), _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val sourceTensor = new TENSOR(tensor, useOldMetadata = true)
      val count = numeral(sourceTensor.resultType.shapeSize)
      check_gpu_array(new ARRAY(transform(tensor)), count, sourceTensor.et, myNCCLRank, name, xs.map(transform):_*)
      Backend.Const(())

    case Node(s, "tensor_result", tt::anno::(x:Backend.Sym)::Backend.Const(i:Int)::_, _) => // subst(s)
      TENSORS.handleTupleView(Adapter.g.globalDefsCache.get(transform(x).asInstanceOf[Backend.Sym]))(xs => xs(i))

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

  override def transform(graph: Graph): Graph = {
    assert (g == null)
    g = new GraphBuilderOpt()
    Adapter.g = g

    try {
      val analysis = new DistributeTensor2MPI_NCCLAnalysis
      analysis.apply(graph)
      hasCublas = analysis.hasCublas
      hasCudnn = analysis.hasCudnn

      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
