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
  import PrimitiveTypeLess._

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_allreduce", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(input:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val inputArray = new ARRAY(transform(input))
      val count = numeral(tt.shapeSize)
      val m = (new TENSOR(input, useOldMetadata=true)).et
      CUDA_STREAM_SYNCHRONIZE(INT(0))
      NCCL_ALLREDUCE(m, inputArray, inputArray, SIZE_T(numeral(tt.shapeSize)), NCCL_SUM, myNCCLComm, myNCCLStream)
      CUDA_STREAM_SYNCHRONIZE(myNCCLStream)
      inputArray.x

    case Node(s, "tensor_send", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) => {
      implicit val pos = Adapter.oldSourceMap(s)
      val recvmodule = recvmap(tag)
      val (dst_start, dst_size) = modulemap(recvmodule)
      val (src_start, src_size) = modulemap(curModule)
      assert(src_size == dst_size)
      val send_tensor = new TENSOR(x, true)
      val count = numeral(send_tensor.resultType.shapeSize)
      val m = send_tensor.et
      // calculate the peer node, currently only support simple topology (doest not support gather/scatter/reduce).
      val peer = if (dst_start > src_start) globalNCCLRank + (dst_start - src_start)
                 else globalNCCLRank - (src_start - dst_start)
      CUDA_STREAM_SYNCHRONIZE(INT(0))
      NCCL_CHECK(NCCL_SEND(m, new ARRAY(transform(x)), SIZE_T(count), INT(peer), globalNCCLComm, myNCCLStream))
      //CUDA_STREAM_SYNCHRONIZE(myNCCLStream)
      Backend.Const(())
    }

    case Node(s, "tensor_recv", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) => {
      implicit val pos = Adapter.oldSourceMap(s)
      val sendmodule = sendmap(tag)
      val (dst_start, dst_size) = modulemap(sendmodule)
      val (src_start, src_size) = modulemap(curModule)
      assert(src_size == dst_size)
      val recv_tensor = new TENSOR(x, true)
      val count = numeral(recv_tensor.resultType.shapeSize)
      val m = recv_tensor.et
      // calculate the peer node, currently only support simple topology (doest not support gather/scatter/reduce).
      val peer = if (dst_start > src_start) globalNCCLRank + (dst_start - src_start)
                 else globalNCCLRank - (src_start - dst_start)
      // x should not be TENSORARRAY.get
      CUDA_STREAM_SYNCHRONIZE(INT(0))
      NCCL_CHECK(NCCL_RECV(m, new ARRAY(transform(x)), SIZE_T(count), INT(peer), globalNCCLComm, myNCCLStream))
      CUDA_STREAM_SYNCHRONIZE(myNCCLStream)
      Backend.Const(())
    }
    case _ => super.transform(n)
  }
}
