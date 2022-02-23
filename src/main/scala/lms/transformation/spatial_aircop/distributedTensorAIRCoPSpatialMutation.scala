package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._


trait DistributeTensorAIRCoPSpatialMutation extends DistributeTensorAIRCoPSpatialBase {

  import PrimitiveTypeLess._
  import FixedSizeDistributedTensorTypeLess._

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "accum_tensor", Backend.Const(anno:Anno)::(base:Backend.Exp)::(addition:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val base_operand = get_operand(base, anno)
      val addition_operand = get_operand(addition, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in accum_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw) for now, assume that `devices` are all the devices
          Accumulate(base_operand, addition_operand).x
        case _ => throw new Exception(s"TODO: $anno")
      }

    case Node(s, "optimize_tensor", Backend.Const(anno:Anno)::(weight:Backend.Exp)::(grad:Backend.Exp)::(momentum:Backend.Exp)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val weight_operand = get_operand(weight, anno)
      val grad_operand = get_operand(grad, anno)
      val momentum_operand = get_operand(momentum, anno)

      anno match {
        case NAnno => throw new Exception(s"TODO: not yet handling NAnno in optimize_tensor")
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          // FIXME(feiw) for now, let's assum that `anno` is for all devices
          Optimize(weight_operand, grad_operand, momentum_operand).x
        case a => throw new Exception(s"TODO: annotation $a is not yet handled in optimize_tensor")
      }

    case Node(s, "all_reduce_tensor", Backend.Const(devices:Seq[Device])::(x:Backend.Sym)::Backend.Const(mode:String)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      // FIXME(feiw) assuming that `devices` are all devices
      val x_operand = new TENSOR(transform(x))
      AllReduceInPlace(x_operand, devices, mode).x

    case Node(s, "tensorarray_zeros", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::Backend.Const(length:Int)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      anno match {
        case NAnno => if (myMPIRank == 0) ZEROS(tt, NAnno).x else Backend.Const(())
        case SAnno(dim: Dim, devices: Seq[Device], _) =>
          ZEROSARRAY(tt.splitBy(anno), NAnno, length).x
          // no need to all reduce, all 0s.
        case a => throw new Exception(s"annotation $a is not yet handled in tensorarray_zeros")
      }
    case Node(s, "tensor_array_get", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::(i:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val operand_array = new TENSORARRAY(x, useOldMetadata = true)
      val operand_anno = operand_array.annotation
      if (anno != operand_anno) {
        throw new Exception(s"Annotation ${anno} and ${operand_anno} mismatch!");
      }
      val array = new TENSORARRAY(transform(x))
      val index = new INT(transform(i))
      TENSORARRAY.get(array, index).x
    case Node(s, "tensor_array_set", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::(x:Backend.Sym)::(i:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val operand_array = new TENSORARRAY(x, useOldMetadata = true)
      val operand_anno = operand_array.annotation
      if (anno != operand_anno) {
        throw new Exception(s"Annotation ${anno} and ${operand_anno} mismatch!");
      }
      val array = new TENSORARRAY(transform(x))
      val index = new INT(transform(i))
      val value = get_operand(y, anno)
      TENSORARRAY.set(array, index, value).x
    case _ => super.transform(n)
  }
}
