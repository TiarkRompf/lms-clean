package lms.transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.array_computation.{ArrayCPUTypeLess, ArrayCPUOps}

import Backend._

// lower Tensor computations to Array computations
abstract class TensorLoweringCPU extends Transformer {

  import BaseTypeLess._
  import PrimitiveTypeLess._
  import ArrayTypeLess._
  import ArrayCPUTypeLess._
  import FixedSizeTensorTypeLess._

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  override def transform(n: Node): Backend.Exp = n match {

    case Node(s, "tensor", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::_, _) =>
      transform(x)

    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      // `oldSourceMap` is the copy of Adapter.sourceMap that maps Backend.Exp to SourceContext
      // We use it to get the SourceContext of the node `s` and use it for the transformed node.
      // `oldTypeMap` is the copy of Adapter.typeMap that maps Backend.Exp to type Manifest
      // We use it to get the type Manifest that is used for typeless frontend (such as for NEW_ARRAY)
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      val m = Adapter.oldTypeMap(s)
      val count = numeral(size)

      // Note that when construction new IR nodes from the old IR nodes, we choose
      // to use the typeless frontend because typeless frontend doesn't have to revive the type.
      // It can work with type manifest directly.
      val res = ARRAY(count, m)

      // Again, we use typeless frontend here
      // We can also use the `unsafe constructor` of `ARRAY` because all arrays metapipe are already registered.
      ARRAY_ADD(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(count))
      res.x

    case Node(s, "tensor_minus", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      val m = Adapter.oldTypeMap(s)
      val count = numeral(size)

      val res = ARRAY(count, m)

      ARRAY_MINUS(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(count))
      res.x

    case Node(s, "tensor_mult", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      val m = Adapter.oldTypeMap(s)
      val count = numeral(size)

      val res = ARRAY(count, m)

      ARRAY_MULT(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(count))
      res.x

    case Node(s, "tensor_div", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      val m = Adapter.oldTypeMap(s)
      val count = numeral(size)

      val res = ARRAY(count, m)

      ARRAY_DIV(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(count))
      res.x

    case Node(s, "tensor_dot", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = Adapter.oldSourceMap(s)
      val m = Adapter.oldTypeMap(s)
      val count = numeral(size)

      val x_shape = (new TENSOR(x, old = true)).shape
      val y_shape = (new TENSOR(y, old = true)).shape

      val res = ARRAY(count, m)

      // match case on the type (input shapes) of the dot
      if (x_shape.size == 1 && y_shape.size == 1) {
          ARRAY_VVDOT(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(x_shape(0)))
      } else if (x_shape.size == 2 && y_shape.size == 1) {
          ARRAY_MVDOT(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(x_shape(0)), INT(x_shape(1)))
      } else if (x_shape.size == 2 && y_shape.size == 2) {
          ARRAY_MMDOT(new ARRAY(transform(x)), new ARRAY(transform(y)), res, INT(x_shape(0)), INT(x_shape(1)), INT(y_shape(1)))
      } else {
          throw new Exception("dot for higher than 2D is not yet supported")
      }

      res.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = Adapter.oldSourceMap(s)

      val shape = (new TENSOR(x, old = true)).shape

      // this unsafe ARRAY construction should be safe because the ARRAY is already constructed with metadata
      // Using typeless frontend for printing
      ARRAY_PRINT(new ARRAY(transform(x)), INT(numeral(shape)))

      Backend.Const(())

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
