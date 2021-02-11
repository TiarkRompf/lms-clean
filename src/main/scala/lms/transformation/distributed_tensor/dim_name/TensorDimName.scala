package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}
import lms.transformation.util.DataStructure

import Backend._


// lower Tensor computations to Array computations
// respect device allocation (simple distinction of GPU and CPU)
abstract class DistributeTensorDimName extends Transformer with DataStructure {
  override val name = "DistributedTensorDimName"

  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeDistributedTensorTypeLess._
  import CUDATypeLess._
  import CUBLASTypeLess._

  // set up a Dim name USets
  var dim_names: USets[Dim] = null

  override def traverse(ns: Seq[Node], res: Block): Unit = {
    // add some analysis here before traversing each node
    // Step 1: collect all dim names in this block
    val all_dims = scala.collection.mutable.ArrayBuffer[Dim]()
    ns.foreach(n => n match {
      case Node(s, op, _, _) if op.startsWith("tensor") =>
        val tensor_type = (new TENSOR(s, useOldMetadata = true)).tensor_type
        all_dims ++= tensor_type.shape.map(_.dim)
      case _ => ()
    })
    dim_names = new USets[Dim](all_dims.toList)

    // Step 2: analyze the dim name relations here (i.e. operation enforces dims
    //         to have the same size)
    ns.foreach(n => mergable_dims(n).foreach { case (d0, d1) => dim_names.merge(d0, d1) })

    // Step 3: traversing each node to update dim names
    ns.foreach(traverse)
  }

  def update_dim_name(tt: TensorType): TensorType = TensorType(tt.shape.map{case Size(d, s) =>
        Size(dim_names.union_map(d), s)}, tt.et, update_dim_name(tt.anno))
  def update_dim_name(anno: Anno): Anno = anno match {
    case SAnno(dim: Dim, devices, _) => SAnno(dim_names.union_map(dim), devices)
    case a => a
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      INPUT(update_dim_name(tt), update_dim_name(anno)).x

    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      WEIGHT(update_dim_name(tt), update_dim_name(anno)).x
    
    case Node(s, op, tt::Backend.Const(anno:Anno)::(x:Backend.Sym)::(y:Backend.Sym)::_, _) if (op.startsWith("tensor_")) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      // this reconstruction should use new dim names in tensor type :)
      op match {
        case "tensor_add" => Add(new TENSOR(transform(x)), new TENSOR(transform(y)), update_dim_name(anno)).x
        case "tenosr_minus" => Sub(new TENSOR(transform(x)), new TENSOR(transform(y)), update_dim_name(anno)).x
        case "tensor_mult" => Mul(new TENSOR(transform(x)), new TENSOR(transform(y)), update_dim_name(anno)).x
        case "tensor_div" => Div(new TENSOR(transform(x)), new TENSOR(transform(y)), update_dim_name(anno)).x
        case "tensor_dot" => Dot(new TENSOR(transform(x)), new TENSOR(transform(y)), update_dim_name(anno)).x
        case _ => throw new Exception(s"op $op is not yet handled in dim name transform")
      }
    
    case Node(s, op, tt::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) if (op.startsWith("tensor_")) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      op match {
        case "tensor_negate" => Negate(new TENSOR(transform(x)), update_dim_name(anno)).x
        case "tensor_invert" => Invert(new TENSOR(transform(x)), update_dim_name(anno)).x
        case _ => throw new Exception(s"op $op is not yet handled in dim name transform")
      }

    case Node(s, op, tt::Backend.Const(anno:Anno)::(x:Backend.Sym)::_, _) if (op.startsWith("tensor_")) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      op match {
        case "tensor_negate" => Negate(new TENSOR(transform(x)), update_dim_name(anno)).x
        case _ => throw new Exception(s"op $op is not yet handled in dim name transform")
      }

    case Node(s, "tensor_dot_with_transpose", tt::Backend.Const(anno:Anno)::Backend.Const(transL:Boolean)::Backend.Const(transR:Boolean)::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      DotWithTranspose(new TENSOR(transform(x)), new TENSOR(transform(y)), update_dim_name(anno), transL, transR).x

    case Node(s, op, _, _) if (op.startsWith("tensor_")) =>
      throw new Exception(s"op $op is not yet handled in dim name transform")

    case _ => super.transform(n)
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
