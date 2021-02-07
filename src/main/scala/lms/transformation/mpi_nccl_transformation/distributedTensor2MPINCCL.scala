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


abstract class DistributeTensor2MPI_NCCL extends DistributeTensor2MPI_NCCLBase
    with DistributeTensor2MPI_NCCLBinary
    with DistributeTensor2MPI_NCCLGemm
    with DistributeTensor2MPI_NCCLMutation
    with DistributedTensor2MPI_NCCLUnary

class DistributeTensor2MPI_NCCLAnalysis extends Traverser {
    var hasCublas = false
    var hasCudnn = false

    override def traverse(n: Node): Unit = n match {
        case Node(s, op, _, _) if op.startsWith("tensor_dot") =>
            hasCublas = true
            super.traverse(n)
        case _ => super.traverse(n)
    }
}
