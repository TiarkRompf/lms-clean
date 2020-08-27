package lms
package transformation

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext
import lms.thirdparty.ArrayCPUOps

import Backend._

// I made this a trait because I want to use them with Rep[T]
trait FixedSizeTensorFrontEnd extends Base with PrimitiveOps with ArrayOps {

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  case class TENSOR(x: E) {
    def withSource(pos: SourceContext) = { Adapter.sourceMap(x) = pos; this }
    def withEleType(m: Manifest[_]) = { Adapter.typeMap(x) = m; this }
    def with_(pos: SourceContext, m: Manifest[_]) = withSource(pos).withEleType(m)

    def p: SourceContext = Adapter.sourceMap.getOrElse(x, ???)
    def et: Manifest[_] = Adapter.typeMap.getOrElse(x, ???)

    def shape: Seq[Int] = x match {
      case Adapter.g.Def(s, Backend.Const(size:Seq[Int])::_) if s.startsWith("tensor") => size
      case a => System.out.println(a); ???
    }
    def show(implicit __pos: SourceContext): UNIT = {
      UNIT(Adapter.g.reflectWrite("show_tensor", x)(Adapter.CTRL))
    }

    def + (y: TENSOR)(implicit __pos: SourceContext): TENSOR = {
      assert(shape == y.shape)
      assert(et == y.et)
      TENSOR(Adapter.g.reflect("tensor_add", C(shape), x, y.x)).with_(__pos, et)
    }
  }

  def TENSOR(shape: Seq[Int], array: ARRAY)(implicit __pos: SourceContext): TENSOR = {
    TENSOR(Adapter.g.reflect("tensor", C(shape), array.x)).with_(__pos, array.et)
  }

  class Tensor[+T]
  object Tensor {
    def apply[T:Numeric:Manifest](shape: Seq[Int], array: Rep[Array[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      Wrap[Tensor[T]](TENSOR(shape, ARRAY(Unwrap(array))).x)
    }
  }

  implicit class TensorOps[T:Numeric:Manifest](x: Rep[Tensor[T]]) {
    def shape: Seq[Int] = TENSOR(Unwrap(x)).shape
    def show(implicit __pos: SourceContext): Rep[Unit] = Wrap[Unit](TENSOR(Unwrap(x)).show.x)
    def + (y: Rep[Tensor[T]])(implicit __pos: SourceContext): Rep[Tensor[T]] = {
      val t = TENSOR(Unwrap(x)) + TENSOR(Unwrap(y))
      Wrap[Tensor[T]](t.x)
    }
  }
}

// lower Tensor computations to Array computations
abstract class TensorLoweringCPU extends Transformer with ArrayCPUOps with FixedSizeTensorFrontEnd {

  val tensor2array = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  // FIXME(feiw) what do we do about different types? Int Float?
  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::_, _) =>
      tensor2array(s) = transform(x).asInstanceOf[Backend.Sym]
      s
    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::_, _) =>
      implicit val sc_ : SourceContext = oldSourceMap(s)
      val res = NEW_ARRAY(numeral(size), oldTypeMap(s))
      tensor2array(s) = res.x.asInstanceOf[Backend.Sym]
      ARRAY_ADD(ARRAY(tensor2array(x)), ARRAY(tensor2array(y)), res, INT(numeral(size)))
      res.x

    case Node(s, "show_tensor", (x: Backend.Sym)::Nil, _) =>
      implicit val sc_ = oldSourceMap(s)
      graphCache.get(x) match {
        case Some(Node(s, op, Backend.Const(size:Seq[Int])::_, _)) =>
          val arr = Wrap[Array[Int]](tensor2array(x))
          array_print[Int](arr, numeral(size))
          Backend.Const(())
        case a => System.out.println(a); ???
      }

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
