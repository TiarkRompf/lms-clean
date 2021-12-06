package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, CUDATypeLess, CUBLASTypeLess}

import Backend._

abstract class FusedTensorFunctional extends Transformer {

  override val name = "FusedTensorFunctional"

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FusedTensorTypeLess._
  import PrimitiveTypeLess._

  val splits = new mutable.HashMap[(Backend.Sym, Int), TENSOR] // source sym, idx |-> TENSOR

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_zeros", (Backend.Const(sz:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(Seq(View(null, 0, sz)))(i => INT(0).x)
      t.x
    case Node(s, "tensor_ones", (Backend.Const(sz:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(Seq(View(null, 0, sz)))(i => INT(1).x)
      t.x
    case Node(s, "tensor_consts", (Backend.Const(sz:Int))::(Backend.Const(n:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(Seq(View(null, 0, sz)))(i => INT(n).x)
      t.x
    case Node(s, "tensor_input", (Backend.Const(sz:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t1 = INPUT1(Seq(View(s, 0, sz))) // necessary?
      t1.x
    case Node(s, "tensor_add", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x))
      val b = new TENSOR(transform(y))
      val t = TENSOR(a.inputs ++ b.inputs){ i =>
        (a.apply(INT(i).x) + b.apply(INT(i).x)).x }
      t.x
    case Node(s, "tensor_minus", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x))
      val b = new TENSOR(transform(y))
      val t = TENSOR(a.inputs ++ b.inputs){ i =>
        (a.apply(INT(i).x) - b.apply(INT(i).x)).x }
      t.x
    case Node(s, "tensor_tanh", (x:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(transform(x))
      val res = TENSOR(t.inputs){ i => t.apply(INT(i).x).tanh().x } // ad-hoc!!!
      res.x
    case Node(s, "tensor_relu", (x:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(transform(x))
      val res = TENSOR(t.inputs){ i =>
        // IF(c: BOOL)(a: => TOP)(b: => TOP)
        (IF(t.apply(INT(i).x) < INT(0))(INT(0))(t.apply(INT(i).x))).x }
      res.x
    case Node(s, "tensor_split", (x:Backend.Sym)::(Backend.Const(sz:Seq[Int]))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(x) // get tensor in this level
      require(t.inputs.size == 1, "tensor to be splitted must have a single input")
      val input = t.inputs(0).t
      require(sz.sum == t.size, "invalid split pattern")

      val froms = sz.init.scanLeft(0) { _ + _ }
      val tos = sz.scanLeft(0) { _ + _ }.tail
      val indices = Range(0, sz.length, 1).toList

      froms zip tos zip indices foreach {
        case ((from, to), i) =>
          splits((s, i)) = TENSOR(Seq(View(input, from, to))){ i => t.apply(INT(i).x).x }
      }
      Backend.Const(())
    
    case Node(s, "tensor_result",(x:Backend.Sym)::(Backend.Const(i:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = splits((x, i))
      t.x
    /*case Node(s, "tensor_concat", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x))
      val b = new TENSOR(transform(y))
      
      require(a.inputs(0).t == b.inputs(0).t, "cannot concat from different sources")
      require(a.inputs(0).to == b.inputs(0).from, "cannot concat unmatched shapes")

      val Backend.Block(a_arg::Nil, a_r, _, _) = a.body
      val Backend.Block(b_arg::Nil, b_r, _, _) = b.body

      val sz = a.size + b.size
      val res = TENSOR(Seq(View(a.inputs(0).t, 0, sz))){ i =>
        (IF(INT(i) < INT(a.size))(a.apply(i))(b.apply(i))).x
      }
      res.x*/
    case Node(s, "tensor_concat", (xs:List[Backend.Sym]), _) =>
      implicit val pos = Adapter.oldSourceMap(s)

      val tensors = xs map { t => new TENSOR(transform(t)) }
      val inputs = tensors map { t =>
        val real_inputs = t.inputs.filter { i => i.t != null }
        require(real_inputs.size == 1, "need only 1 input for concat")
        real_inputs(0)
      }
      val sorted = (tensors zip inputs).sortWith(_._2.from < _._2.from)
      // System.out.println(sorted)

      val s_tensors = sorted map { _._1 }
      val s_inputs = sorted map { _._2 }

      // System.out.println(s_tensors)
      // System.out.println(s_inputs)

      val input = s_inputs(0).t
      s_inputs foreach { v => require(v.t == input, "cannot concat from different sources") }

      // TODO: rewrite me
      var end = s_inputs.head.to
      for (v <- s_inputs.tail) {
        if (v.from != end) {
          require(false, "cannot merge range")
        } else {
          end = v.to
        }
      }
      // System.out.println(end)

      // val gaps = sorted.init map { _._2.to }
      val gaps = s_inputs.init map { _.to }
      // System.out.println(gaps)

      val res = TENSOR(Seq(View(input, 0, end))){ i =>
        /*
        val inner1 = sorted(2)._1.apply(i)
        val inner = IF(INT(i) < INT(gaps(1)))(sorted(1)._1.apply(i))(inner1)
        (IF(INT(i) < INT(gaps(0)))(sorted(0)._1.apply(i))(inner)).x*/
        var inner = s_tensors(gaps.size).apply(i)
        var j = gaps.size - 1
        while (j >= 0) {
          inner = INT(IF(INT(i) < INT(gaps(j)))(s_tensors(j).apply(i))(inner))
          j = j - 1
        }
        inner.x
      }
      res.x
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
