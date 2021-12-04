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
    case Node(s, "tensor_zeros", (Backend.Const(sz:Seq[Int]))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(sz, Seq())(i => INT(0).x)
      t.x
    case Node(s, "tensor_ones", (Backend.Const(sz:Seq[Int]))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(sz, Seq())(i => INT(1).x)
      t.x
    case Node(s, "tensor_consts", (Backend.Const(sz:Seq[Int]))::(Backend.Const(n:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = TENSOR(sz, Seq())(i => INT(n).x)
      t.x
    case Node(s, "tensor_input", (Backend.Const(sz:Seq[Int]))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t1 = INPUT1(sz, Seq(s)) // necessary?
      t1.x
    case Node(s, "tensor_add", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x))
      val b = new TENSOR(transform(y))
      val t = TENSOR(a.size, a.inputs ++ b.inputs){ i =>
        (a.apply(INT(i).x) + b.apply(INT(i).x)).x }
      t.x
    case Node(s, "tensor_minus", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x))
      val b = new TENSOR(transform(y))
      val t = TENSOR(a.size, a.inputs ++ b.inputs){ i =>
        (a.apply(INT(i).x) - b.apply(INT(i).x)).x }
      t.x
    case Node(s, "tensor_tanh", (x:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(transform(x))
      val res = TENSOR(t.size, t.inputs){ i => t.apply(INT(i).x).tanh().x } // ad-hoc!!!
      res.x
    case Node(s, "tensor_relu", (x:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(x)
      val res = TENSOR(t.size, t.inputs){ i =>
        // IF(c: BOOL)(a: => TOP)(b: => TOP)
        (IF(t.apply(INT(i).x) < INT(0))(INT(0))(t.apply(INT(i).x))).x }
      res.x
    case Node(s, "tensor_split", (x:Backend.Sym)::(Backend.Const(sz:Seq[Int]))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = new TENSOR(x) // get tensor in this level
      // val sz = t.size
      require(sz.sum == t.size.sum, "invalid split pattern")

      val t1 = TENSOR(Seq(0, sz(0)), t.inputs){ i => t.apply(INT(i).x).x } // fixme: sizes are ad-hoc
      val t2 = TENSOR(Seq(sz(0), t.size.sum), t.inputs){ i => t.apply(INT(i).x).x }
      splits((s, 0)) = t1
      splits((s, 1)) = t2
      // System.out.println("t: " + t)
      // System.out.println("t1: " + t1)
      // System.out.println("t2: " + t2)
      
      //TENSORS(Seq(t1.x, t2.x)).x
      Backend.Const(())
    
    case Node(s, "tensor_result",(x:Backend.Sym)::(Backend.Const(i:Int))::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val t = splits((x, i))
      t.x
    case Node(s, "tensor_concat", (x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val pos = Adapter.oldSourceMap(s)
      val a = new TENSOR(transform(x))
      val b = new TENSOR(transform(y))

      require(a.size.last == b.size.head && a.inputs == b.inputs, "cannot concat")

      // System.out.println("a: " + a.body)
      // System.out.println("b: " + b.body)

      val Backend.Block(a_arg::Nil, a_r, _, _) = a.body
      val Backend.Block(b_arg::Nil, b_r, _, _) = b.body

      val sz = a.size.sum + b.size.sum
      val res = TENSOR(Seq(0, sz), a.inputs){ i =>
        (IF(INT(i) < INT(a.size.sum))(a.apply(i))(b.apply(i))).x
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
