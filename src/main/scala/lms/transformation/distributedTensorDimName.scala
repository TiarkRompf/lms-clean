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
        all_dims ++= tensor_type.s.map(_.d)
      case _ => ()
    })
    dim_names = new USets[Dim](all_dims.toList)

    // Step 2: analyze the dim name relations here (i.e. operation enforces dims
    //         to have the same size)
    ns.foreach(n => n match {
      case Node(s, op, tt::anno::(x:Backend.Sym)::(y:Backend.Sym)::_, _)
          if (op == "tensor_add" || op == "tensor_minus" || op == "tensor_mul" || op == "tensor_div") =>
        val x_type = (new TENSOR(x, useOldMetadata=true)).tensor_type
        val y_type = (new TENSOR(y, useOldMetadata=true)).tensor_type
        (x_type.s zip y_type.s) foreach { case (a:Size, b:Size) =>
          dim_names.merge(a.d, b.d)
        }
      case Node(s, "tensor_dot", tt::anno::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
        val x_type = (new TENSOR(x, useOldMetadata=true)).tensor_type
        val y_type = (new TENSOR(y, useOldMetadata=true)).tensor_type
        (x_type.s.size, y_type.s.size) match {
          case (1,1) => dim_names.merge(x_type.s.head.d, y_type.s.head.d)
          case (2,1) => dim_names.merge(x_type.s.last.d, y_type.s.head.d)
          case (2,2) => dim_names.merge(x_type.s.last.d, y_type.s.head.d)
          case _ => ???
        }
      case Node(s, op, _, _) if (op == "tensor_input" || op == "tensor_weight") => ()
      case n@Node(s, op, _, _) => assert(!op.startsWith("tensor"), s"not handling dim merge in $n")
    })

    // Step 3: traversing each node to update dim names
    ns.foreach(traverse)
  }

  def update_dim_name(tt: TensorType) = TensorType(tt.s.map{case Size(d, s) =>
        Size(dim_names.union_map(d), s)}, tt.d, tt.et)
  def update_dim_name(anno: Anno) = anno match {
    case SAnno(d: Dim, devices) => SAnno(dim_names.union_map(d), devices)
    case a => a
  }

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor_input", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      INPUT(update_dim_name(tt), update_dim_name(anno)).x

    case Node(s, "tensor_weight", Backend.Const(tt:TensorType)::Backend.Const(anno:Anno)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      WEIGHT(update_dim_name(tt), update_dim_name(anno)).x

    case Node(s, op, tt::Backend.Const(anno:Anno)::(x:Backend.Sym)::(y:Backend.Sym)::_, _)
        if (op == "tensor_add" || op == "tensor_minus" || op == "tensor_mult" || op == "tensor_div") =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      // this reconstruction should use new dim names in tensor type :)
      op match {
        case "tensor_add" => ((new TENSOR(transform(x))) + (new TENSOR(transform(y)), update_dim_name(anno))).x
        case "tenosr_minus" => ((new TENSOR(transform(x))) - (new TENSOR(transform(y)), update_dim_name(anno))).x
        case "tensor_mult" => ((new TENSOR(transform(x))) * (new TENSOR(transform(y)), update_dim_name(anno))).x
        case "tensor_div" => ((new TENSOR(transform(x))) / (new TENSOR(transform(y)), update_dim_name(anno))).x
      }

    case Node(s, "tensor_dot", tt::Backend.Const(anno:Anno)::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      ((new TENSOR(transform(x))) dot (new TENSOR(transform(y)), update_dim_name(anno))).x

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
