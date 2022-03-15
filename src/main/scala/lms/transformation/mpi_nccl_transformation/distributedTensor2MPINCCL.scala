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

import FixedSizeDistributedTensorTypeLess._
import Backend._


abstract class DistributeTensor2MPI_NCCL extends DistributeTensor2MPI_NCCLBase
    with DistributeTensor2MPI_NCCLBinary
    with DistributeTensor2MPI_NCCLGemm
    with DistributeTensor2MPI_NCCLMutation
    with DistributeTensor2MPI_NCCLSplitConcat
    with DistributedTensor2MPI_NCCLUnary
    with DistributeTensor2MPI_NCCLConv
    with DistributeTensor2MPI_NCCLMiscs
    with DistributedTensor2MPI_NCCLComm

class DistributeTensor2MPI_NCCLAnalysis extends Traverser {
    var hasCublas = false
    var hasCudnn = false
    var modulecount = 0

    val cudnn_ops = List("tensor_conv", "tensor_softmax", "tensor_activation", "tensors_dropout", "tensor_pooling")

    val modulemap = mutable.HashMap[Backend.Sym, (Int, Int)]()
    val sendmap = mutable.HashMap[String, Backend.Sym]()
    val recvmap = mutable.HashMap[String, Backend.Sym]()
    var curModule:Backend.Sym = null

    override def traverse(n: Node): Unit = n match {
        case Node(s, op, _, _) if op.startsWith("tensor_dot") =>
            hasCublas = true
            super.traverse(n)
        case Node(s, op, _, _) if cudnn_ops.exists(op.startsWith) =>
            hasCudnn = true
            super.traverse(n)
        case Node(s, "module", Backend.Const(manno:Anno)::(b @ Block(in, y, ein, eff))::_, _) => {
          val devices = manno match {
            case a @ MAnno(devices, islastmodule) => devices
            case a @ KAnno(pipeline, devices, islastmodule) => devices
            case _: QAnno => throw new Exception(s"Queued pipeline currently not supported")
            case _ => throw new Exception(s"Not module annotation")
          }
          curModule = s
          modulemap(s) = (modulecount, devices.size)
          modulecount += devices.size
          super.traverse(n)
        }
        case Node(s, "tensor_send", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) =>
          sendmap(tag) = curModule
        case Node(s, "tensor_recv", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) =>
          recvmap(tag) = curModule
        case _ => super.traverse(n)
    }
}
