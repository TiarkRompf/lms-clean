package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._
import scala.collection.mutable.HashMap
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps, CUDNNTypeLess, CLibTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensor2MPI_NCCLBase extends Transformer with MPIOps with CudaOps with NCCLOps with CUDNNOps with PrimitiveOps with OrderingOps {
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
      case m if m == manifest[Float] => "check_float_array_with_file"
      case m if m == manifest[Int] => "check_int_array_with_file"
      case m => throw new Exception(s"not yet supporting manifest ${m}")
    }
    LIB_FUNCTION(manifest[Unit], function, (check.x)::Backend.Const(count)::Unwrap(filenameFormat)::filenameArgs.map(Unwrap).toList:_*)(Seq[Int](0), Seq[Int](), Set[Int](), Adapter.CTRL)
  }

  // helper function for initializing a GPU array from binary file
  def gpu_scanner_array(name: String, size: Int, m: Manifest[_], device: INT)(implicit __pos: SourceContext): ARRAY =
    withComment(s"initializing GPU array of size $size and type $m at device (pre-rename) ${device.x} from binary file ${name}") {
      val cpuArray = cpu_array(size, m)
      val gpuArray = gpu_array(size, m, myCUDADevice)
      ScanFileRank(unit("golden/" + name), device, cpuArray, size)
      CUDA_MEMCPY(gpuArray, cpuArray, size, HOST2DEVICE, m)
      gpuArray
    }
  // helper function for initializing a GPU array
  def gpu_scanner_array(size: Int, m: Manifest[_], device: INT, filenameFormat: String, filenameArgs: TOP*)(implicit __pos: SourceContext): ARRAY = {
    withComment(s"initializing GPU array of size $size and type $m") {
      val cpuArray = cpu_array(size, m)
      val gpuArray = gpu_array(size, m, myCUDADevice)
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
  lazy val (myNCCLSizeRep, myNCCLRankRep, globalNCCLSizeRep, globalNCCLRankRep, myNCCLCommRep, globalNCCLCommRep, myNCCLStreamRep) = withComment("setting up the MPI/NCCL environment") {

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

    if (modulemap != null) withComment("setting up the local MPI/NCCL environment") {
      val new_comm = mpi_comm
      val values = modulemap.map{ case (key, (v1, v2)) => v2}.toList.toSet
        if (values.size != 1) {
          throw new Exception(s"All module must have equal number of devices")
      }
      //implicit val pos:SourceContext = SourceContext._sc
      //implicit val overloaded80 = new Overloaded80
      val msize:Int = values.head
      val read_rank = readVar(rank)
      // split the global communicator
      val color = Wrap[Int](Adapter.g.reflect("/", Unwrap(read_rank), Backend.Const(msize)))
      MPI_CHECK(mpi_comm_split(mpi_comm_world, color, read_rank, new_comm))
      // get the local size and rank
      val local_size = var_new(unit(0))
      val local_rank = var_new(unit(0))
      MPI_CHECK(mpi_comm_rank(new_comm, local_rank))
      MPI_CHECK(mpi_comm_size(new_comm, local_size))

      val local_commId = ncclUniqueId
      ncclCheck(ncclGetUniqueId(local_commId))
      MPI_CHECK(mpi_bcast_one(local_commId, ncclUniqueIdBytes, mpi_char, unit(0), new_comm))

      val local_ncclcomm = ncclComm
      ncclCheck(ncclCommInitRank(local_ncclcomm, readVar(local_size), local_commId, readVar(local_rank)))
      (readVar(local_size), readVar(local_rank), readVar(size), readVar(rank), local_ncclcomm, comm, stream)
    } else {
      val mySize = readVar(size)
      val myRank = readVar(rank)
      (mySize, myRank, mySize, myRank, comm, comm, stream)
    }
  }

  // Local NCCL Size and Rank (for each module).
  def myNCCLSize(implicit __pos: SourceContext) = INT(Unwrap(myNCCLSizeRep))
  def myNCCLRank(implicit __pos: SourceContext) = INT(Unwrap(myNCCLRankRep))
  // Global NCCL Size and Rank.
  def globalNCCLSize(implicit __pos: SourceContext) = INT(Unwrap(globalNCCLSizeRep))
  def globalNCCLRank(implicit __pos: SourceContext) = INT(Unwrap(globalNCCLRankRep))
  def myNCCLComm(implicit __pos: SourceContext) = TOP(Unwrap(myNCCLCommRep), manifest[ncclCommT])
  def globalNCCLComm(implicit __pos: SourceContext) = TOP(Unwrap(globalNCCLCommRep), manifest[ncclCommT])
  def myNCCLStream(implicit __pos: SourceContext) = TOP(Unwrap(myNCCLStreamRep), manifest[cudaStreamT])
  def myCUDADevice(implicit __pos: SourceContext) = INT(Unwrap(globalNCCLRankRep))

  def set_up_mpi_nccl(implicit pos: SourceContext) = {
    val dummy = myNCCLSize
    setupCublasCudnn(pos)
    //setup_profiler(pos)
  }
  def finalize_mpi_nccl(implicit pos: SourceContext) = {
    //finalize_profiler(pos)
    ncclCheck(ncclCommDestroy(myNCCLCommRep))
    if (modulemap != null)
      ncclCheck(ncclCommDestroy(globalNCCLCommRep))
    finalizeCublasCudnn(pos)
    MPI_CHECK(mpi_finalize())
  }

  var hasCublas = false
  var hasCudnn = false
  var curModule:Backend.Sym = null
  var modulemap:immutable.Map[Backend.Sym, (Int, Int)] = null
  var sendmap:immutable.Map[String, Backend.Sym] = null
  var recvmap:immutable.Map[String, Backend.Sym] = null
  lazy val (start_event, stop_event) = {
    val start = cudaEvent
    val stop = cudaEvent
    MPI_CHECK(mpi_barrier(mpi_comm_world))
    cudaCall(cudaEventCreate(start))
    cudaCall(cudaEventCreate(stop))
    cudaCall(cudaEventRecord(start))
    cudaProfilerStart()
    (start, stop)
  }

  def setupCublasCudnn(implicit pos: SourceContext): Unit = {
    if (hasCublas) set_up_cublas
    if (hasCudnn) set_up_cudnn
  }
  def finalizeCublasCudnn(implicit pos: SourceContext): Unit = {
    if (hasCublas) finalize_cublas
    if (hasCudnn) finalize_cudnn
  }

  def setup_profiler(implicit pos: SourceContext):Unit = {
    (start_event, stop_event)
    ()
  }
  def finalize_profiler(implicit pos: SourceContext):Unit = {
    cudaCall(cudaEventRecord(stop_event))
    cudaCall(cudaEventSynchronize(stop_event))
    cudaProfilerStop();
    val time = var_new[Float](unit(0.0f))
    cudaCall(cudaEventElapsedTime(time, start_event, stop_event))
    printf("cuda event duration: %f\n", readVar(time));
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
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      set_up_mpi_nccl
      myNCCLRank.x

    // track the world_size to set up MPI_NCCL
    case Node(s, "world_size", _, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      set_up_mpi_nccl
      myNCCLSize.x

    // track the global_rank to set up MPI_NCCL
    case Node(s, "global_rank", _, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      set_up_mpi_nccl
      globalNCCLRank.x

    // track the global_size to set up MPI_NCCL
    case Node(s, "global_size", _, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      set_up_mpi_nccl
      globalNCCLSize.x

    // track the world_finalize to finalize the MPI_NCCL
    case Node(s, "world_finalize", _, _) =>
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      finalize_mpi_nccl
      Backend.Const(())

    case Node(s, "module", Backend.Const(manno: Anno)::(b @ Block(in, y, ein, eff))::_, _) => {
      implicit val pos: SourceContext = Adapter.oldSourceMap(s)
      if (modulemap == null) {
        traverse(b)
        UNIT(Backend.Const(())).x
      } else {
        curModule = s
        // module NCCLRank start and end
        IF (globalNCCLRank >= modulemap(s)._1 && (globalNCCLRank < modulemap(s)._1 + modulemap(s)._2)) {
          traverse(b)
          UNIT(Backend.Const(()))
        } { UNIT(Backend.Const(())) }.x
      }
    }

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_scanner_array(count, sourceTensor.et, myCUDADevice, filenameFormat, filenameArgs.map(arg=>new TOP(transform(arg))):_*).x

    case Node(s, "tensor_weight", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_random_array(count, sourceTensor.et, myCUDADevice).x

    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_scanner_array(count, sourceTensor.et, myCUDADevice, filenameFormat, filenameArgs.map(arg=>new TOP(transform(arg))):_*).x

    case Node(s, "tensor_input", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_random_array(count, sourceTensor.et, myCUDADevice).x

    case Node(s, "tensor_zeros", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_fixed_array(count, myCUDADevice, NUM(Backend.Const(0), sourceTensor.et)).x

    case Node(s, "tensor_ones", Backend.Const(tt: TensorType)::Backend.Const(anno: Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val count = numeral(sourceTensor.shapeSize)
      gpu_fixed_array(count, myCUDADevice, NUM(Backend.Const(1), sourceTensor.et)).x

    case Node(s, "save_tensor", (tensor:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val sourceTensor = new TENSOR(tensor, useOldMetadata = true)
      val m = sourceTensor.et
      val tt = sourceTensor.resultType
      val anno = sourceTensor.annotation

      val count = numeral(sourceTensor.shapeSize)
      gpu_to_cpu_and_print(count, m, transform(tensor))
      Backend.Const(())

    case Node(s, "check_tensor", (tensor:Backend.Exp)::Backend.Const(name:String)::(xs:List[Backend.Exp]), _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val sourceTensor = new TENSOR(tensor, useOldMetadata = true)
      val count = numeral(sourceTensor.resultType.shapeSize)
      check_gpu_array(new ARRAY(transform(tensor)), count, sourceTensor.et, myCUDADevice, name, xs.map(transform):_*)
      Backend.Const(())

    case Node(s, "tensor_result", tt::anno::(x:Backend.Sym)::Backend.Const(i:Int)::_, _) => // subst(s)
      TENSORS.handleTupleView(Adapter.g.globalDefsCache.get(transform(x).asInstanceOf[Backend.Sym]))(xs => xs(i))

    case Node(s, op, _, _) if op.startsWith("tensor_") || op.startsWith("tensors_") =>
      throw new Exception(s"not yet handling $n in distributedTensor2MPINCCL transformation")

    case _ => super.transform(n)
  }

  def tensor_shape(tensor: Backend.Exp, useOldMetadata: Boolean = false): Seq[Int] =
    (new TENSOR(tensor, useOldMetadata)).shapeSize

  def tensor_et(tensor: Backend.Exp, useOldMetadata: Boolean = false): Manifest[_] =
    (new TENSOR(tensor, useOldMetadata)).et

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
      modulemap = if (analysis.modulemap.size > 1) analysis.modulemap.toMap else null
      sendmap = if (!analysis.sendmap.isEmpty) analysis.sendmap.toMap else null
      recvmap = if (!analysis.recvmap.isEmpty) analysis.recvmap.toMap else null
      if (modulemap != null) {
        val values = modulemap.map{ case (key, (v1, v2)) => v2}.toList.toSet
        if (values.size != 1) {
          throw new Exception(s"All module must have equal number of devices")
        }
      }

      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
