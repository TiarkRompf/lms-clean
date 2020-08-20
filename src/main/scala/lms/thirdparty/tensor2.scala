package lms
package tensors

import scala.annotation.implicitNotFound
import scala.collection._

import lms.core._
import lms.core.stub._
import lms.collection.mutable._
import lms.macros.SourceContext

import Backend._

// FIXME(feiw) I made this a trait because I want to use them with Rep[T]
trait FixedSizeTensorFrontEnd extends Base {

  type E = Backend.Exp
  def C(a: Any) = Backend.Const(a)

  case class Tensor(x: E) {
    def shape: Seq[Int] = x match {
      case Adapter.g.Def(s, Backend.Const(size:Seq[Int])::_) if s.startsWith("tensor") => size
      case a => System.out.println(a); ???
    }

    def eleType: Manifest[_] = x match {
      case Adapter.g.Def(s, init :+ Backend.Const(m:Manifest[_])) if s.startsWith("tensor") => m
      case a => System.out.println(a); ???
    }

    def show(implicit __pos: SourceContext): Rep[Unit] =
      Wrap[Unit](Adapter.g.reflectWrite("show_tensor", x, C(__pos))(Adapter.CTRL))

    def + (y: Tensor)(implicit __pos: SourceContext): Tensor = {
      assert(shape == y.shape)
      assert(eleType == y.eleType)
      Tensor(Adapter.g.reflect("tensor_add", C(shape), x, y.x, C(__pos), C(eleType)))
    }

  }

  def Tensor[T:Numeric:Manifest](shape: Seq[Int], array: E)(implicit __pos: SourceContext): Tensor =
    Tensor(Adapter.g.reflect("tensor", C(shape), array, C(__pos), C(manifest[T])))
}

trait ArrayCPUOps extends Dsl with ArrayOps {
  def array_add[T:Numeric:Manifest](a: Rep[Array[T]], b: Rep[Array[T]], size: Int)(implicit __pos: SourceContext) = {
    val res = NewArray[T](size)
    for (i <- (0 until size): Rep[Range]) {
      res(i) = a(i) + b(i)
      System.out.println(s"$res = $a + $b")
    }
    res
  }
  def array_print[T:Manifest](a: Rep[Array[T]], size: Int)(implicit __pos: SourceContext) = {
    for (i <- (0 until size): Rep[Range]) {
      printf("%d ", a(i))
    }
  }
}

// lower Tensor computations to Array computations
abstract class TensorLowering2 extends Transformer with ArrayCPUOps with FixedSizeTensorFrontEnd {
  // def init() = {
  //   g = new GraphBuilderOpt()
  //   Adapter.g = g
  // }

  val tensor2array = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  // FIXME(feiw) what do we do about different types? Int Float?
  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::_, _) =>
      tensor2array(s) = transform(x).asInstanceOf[Backend.Sym]
      s
    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::
                 Backend.Const(sc:SourceContext)::Backend.Const(m:Manifest[_])::Nil, _) =>
      implicit val sc_ :SourceContext = sc
      if (m == manifest[Int]) {
        val arr1 = Wrap[Array[Int]](tensor2array(x))
        val arr2 = Wrap[Array[Int]](tensor2array(y))
        val res = array_add[Int](arr1, arr2, numeral(size))
        tensor2array(s) = Unwrap(res).asInstanceOf[Backend.Sym]
        Unwrap(res)
      } else {
        ???
      }

    case Node(s, "show_tensor", (x: Backend.Sym)::Backend.Const(sc:SourceContext)::Nil, _) =>
      implicit val sc_ = sc
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
    g = new GraphBuilderOpt()
    Adapter.g = g
    try {
      super.transform(graph)
    } finally {
      g = null; Adapter.g = null
    }
  }
}
