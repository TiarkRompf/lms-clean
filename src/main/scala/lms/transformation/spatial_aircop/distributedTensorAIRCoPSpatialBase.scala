package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}
import lms.thirdparty.MPIOps
import lms.transformation.util.DataStructure

import Backend._


abstract class DistributeTensorAIRCoPSpatialBase extends Transformer with MPIOps {
  override val name = "DistributeTensorAIRCoPSpatial"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  lazy val (worldSize, worldRank) =
    (Wrap[Int](g.reflectEffect("world_size")()(Adapter.CTRL)), Wrap[Int](g.reflectEffect("world_rank")()(Adapter.CTRL)))

  // lazy local functions that initialize the MPI
  lazy val (myMPISizeRep, myMPIRankRep) = withComment("setting up the MPI environment") {
    val size = var_new(unit(0))
    val rank = var_new(unit(0))
    MPI_CHECK(mpi_init())
    MPI_CHECK(mpi_comm_rank(mpi_comm_world, rank))
    MPI_CHECK(mpi_comm_size(mpi_comm_world, size))
    MPI_CHECK(mpi_barrier(mpi_comm_world))
    (readVar(size), readVar(rank))
  }

  def myMPISize(implicit __pos: SourceContext) = INT(Unwrap(worldSize))
  def myMPIRank(implicit __pos: SourceContext) = INT(Unwrap(worldRank))

  def setup_mpi(implicit __pos: SourceContext) = { val dummy = worldSize }
  def finalize_mpi(implicit __pos: SourceContext) =
    Wrap[Unit](g.reflectEffect("world_finalize")()(Adapter.CTRL))

  def get_operand(operand: Backend.Exp, anno: Anno, assertSame: Boolean = false): TENSOR = {
    val operand_tensor = new TENSOR(operand, useOldMetadata = true)
    val operand_anno = operand_tensor.annotation
    if (operand_anno == anno) {
      new TENSOR(transform(operand))
    } else if (assertSame) {
      throw new Exception(s"Assert that the tensor has the same annotation but it does not: ${operand_anno} v.s. ${anno}");
    } else {
      throw new Exception(s"TODO: not yet handling split annotation conflict $operand_tensor")
    }
  }

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _ ) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val newFilenameFormat = s"golden/${filenameFormat}_rank_%d.data"
      val newFilenameArgs = filenameArgs.map(new TOP(_)) :+ myMPIRank

      anno match {
        case NAnno => if (myMPIRank == 0) WEIGHT(tt, NAnno, newFilenameFormat, newFilenameArgs:_*).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw) how do we know which rank should have this weight?
          val weight = WEIGHT(tt.splitBy(anno), NAnno, newFilenameFormat, newFilenameArgs:_*)
          if (!tt.contains(dim)) {
            AllReduceInPlace(weight, devices, mode="mean")
          }
          weight.x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_weight")
      }

    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _ ) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      anno match {
        case NAnno => if (myMPIRank == 0) WEIGHT(tt, NAnno).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw) how do we know which rank should have this weight?
          val weight = WEIGHT(tt.splitBy(anno), NAnno)
          if (!tt.contains(dim)) {
            AllReduceInPlace(weight, devices, mode="mean")
          }
          weight.x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_weight")
      }

    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(filenameFormat:String)::(filenameArgs:List[Backend.Exp]), _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      val newFilenameFormat = s"golden/${filenameFormat}_rank_%d.data"
      val newFilenameArgs = filenameArgs.map(new TOP(_)) :+ myMPIRank

      anno match {
        case NAnno => if (myMPIRank == 0) INPUT(tt, NAnno, newFilenameFormat, newFilenameArgs:_*).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw) how do we know which rank should have this input?
          val input = INPUT(tt.splitBy(anno), NAnno, newFilenameFormat, newFilenameArgs:_*)
          if (!tt.contains(dim)) {
            AllReduceInPlace(input, devices, mode="mean")
          }
          input.x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_input")
      }

    // FIXME(feiw) input should be P2P communications (such as from CPU to GPU)
    // for now we just try randomization or scan input
    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos

      anno match {
        case NAnno => if (myMPIRank == 0) INPUT(tt, NAnno).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw) how do we know which rank should have this input?
          val input = INPUT(tt.splitBy(anno), NAnno)
          if (!tt.contains(dim)) {
            AllReduceInPlace(input, devices, mode="mean")
          }
          input.x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_input")
      }

    case Node(s, "tensor_zeros", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      anno match {
        case NAnno => if (myMPIRank == 0) ZEROS(tt, NAnno).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw): how do we know which rank should have this zero tensor?
          ZEROS(tt.splitBy(anno), NAnno).x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_zeros")
      }

    case Node(s, "tensor_ones", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_,_) =>
      val sourceTensor = new TENSOR(s, useOldMetadata = true)
      implicit val pos: SourceContext = sourceTensor.pos
      anno match {
        case NAnno => if (myMPIRank == 0) ONES(tt, NAnno).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw): how do we know which rank should have this one tensor?
          ONES(tt.splitBy(anno), NAnno).x
        case a => throw new Exception(s"annotation $a is not yet handled in tensor_ones")
      }

    case Node(s, "save_tensor", (tensor:Backend.Exp)::_, _) => throw new Exception(s"TODO handle save_tensor")

    case Node(s, "check_tensor", (tensor:Backend.Exp)::Backend.Const(name:String)::Nil, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val sourceTensor = new TENSOR(tensor, useOldMetadata = true)
      val tt = sourceTensor.resultType
      val anno = sourceTensor.annotation

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in check_tensor")
        case SAnno(dim:Dim, devices:Seq[Device], _) =>
          (new TENSOR(transform(tensor))).check(s"golden/${name}_rank_%d.data", myMPIRank).x
        case a => throw new Exception(s"annotation $a is not yet handled in check_tensor")
      }

    case Node(s, "tensor_result", tt::anno::(x:Backend.Sym)::Backend.Const(i:Int)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      TENSORS.getResult(new TENSORS(transform(x)), i).x

    case Node(s, op, _, _) if op.startsWith("tensor_") || op.startsWith("tensors_") =>
      throw new Exception(s"not yet handling $n in distributedTensorAIRCoPSpatial transformation")

    case _ => super.transform(n)
  }

  override def wrapGraph(graph: Graph): Unit = {
    implicit val pos: SourceContext = Adapter.oldSourceMap.head._2
    setup_mpi
    super.apply(graph)
    finalize_mpi
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
