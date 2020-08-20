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

  case class Tensor(x: Backend.Exp) {
    def shape: Seq[Int] = x match {
      // FIXME(feiw) 2 problems:
      // 1. one case for all "tensor"*
      // 2. is using g.Def OK? I think not for transformation
      case Adapter.g.Def("tensor", Backend.Const(s:Seq[Int])::_) => s
      case Adapter.g.Def("tensor_add", Backend.Const(s:Seq[Int])::_) => s
      case a => System.out.println(a); ???
    }

    def show(implicit __pos: SourceContext): Rep[Unit] =
      Wrap[Unit](Adapter.g.reflectWrite("tensor_show", x, Backend.Const(__pos))(Adapter.CTRL))

    def + (y: Tensor)(implicit __pos: SourceContext): Tensor = {
      // compute result shape
      assert(shape == y.shape)
      Tensor(Adapter.g.reflect("tensor_add", Backend.Const(shape), x, y.x, Backend.Const(__pos)))
    }

  }

  def Tensor(shape: Seq[Int], array: Backend.Exp): Tensor =
    Tensor(Adapter.g.reflect("tensor", Backend.Const(shape), array))
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

abstract class TensorLowering2 extends Transformer with ArrayCPUOps {
  // lower Tensor computations to Array computations
  // FIXME(feiw) very likely this FrontEnd is not needed?
  val frontEnd: FixedSizeTensorFrontEnd
  import frontEnd._
  def init() = {
    g = new GraphBuilderOpt()
    Adapter.g = g
  }

  val tensor2array = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  // FIXME(feiw) what do we do about different types? Int Float?
  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::Nil, _) =>
      tensor2array(s) = transform(x).asInstanceOf[Backend.Sym]
      s
    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::
        Backend.Const(sc:SourceContext)::Nil, _) =>
      // FIXME(feiw) should do match case on type Manifest?
      implicit val sc_ :SourceContext = sc
      val arr1 = Wrap[Array[Int]](tensor2array(x))
      val arr2 = Wrap[Array[Int]](tensor2array(y))
      val res = array_add[Int](arr1, arr2, numeral(size))
      tensor2array(s) = Unwrap(res).asInstanceOf[Backend.Sym]
      Unwrap(res)

    case Node(s, "tensor_show", (x: Backend.Sym)::Backend.Const(sc:SourceContext)::Nil, _) =>
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
}
