package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.{RandomDataTypeLess, NCCLTypeLess, MPIOps, NCCLOps, SIZE_TTypeLess, CUDNNOps, CUDNNTypeLess, CLibTypeLess}
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess, CudaOps}
import lms.transformation.util.DataStructure

import Backend._

trait DistributedTensor2MPI_NCCLComm extends DistributeTensor2MPI_NCCLBase {

  import ArrayTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  import CUDATypeLess._
  import NCCLTypeLess._
  import SIZE_TTypeLess._

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_allreduce", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val inputArray = new ARRAY(transform(input))
      val count = numeral(tt.shapeSize)
      val m = (new TENSOR(input, useOldMetadata=true)).et
      NCCL_ALLREDUCE(m, inputArray, inputArray, SIZE_T(numeral(tt.shapeSize)), NCCL_SUM, myNCCLComm, myNCCLStream)
      CUDA_STREAM_SYNCHRONIZE(myNCCLStream)
      inputArray.x

    case _ => super.transform(n)
  }
}
