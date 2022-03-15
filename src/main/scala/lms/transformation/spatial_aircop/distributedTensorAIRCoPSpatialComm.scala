package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialComm extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_send", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val send_tensor = get_operand(x, anno)
      SEND(send_tensor, tag).x
    case Node(s, "tensor_recv", Backend.Const(tt:TensorType)::Backend.Const(anno: Anno)::Backend.Const(tag:String)::(x:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val recv_tensor = get_operand(x, anno)
      RECV(recv_tensor, tag)
      recv_tensor.x
    case _ => super.transform(n)
  }
}
