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
trait FixedSizeTensorFrontEnd extends FrontEndT with Base {

  case class Tensor(x: Backend.Exp) {
    def shape: Seq[Int] = x match {
      // FIXME(feiw) 2 problems:
      // 1. one case for all "tensor"*
      // 2. is using g.Def OK? I think not for transformation
      case g.Def("tensor", Backend.Const(s:Seq[Int])::_) => s
      case g.Def("tensor_add", Backend.Const(s:Seq[Int])::_) => s
      case a => System.out.println(a); ???
    }

    def show: Rep[Unit] = Wrap[Unit](g.reflectEffect("tensor_show", x)()(Adapter.CTRL))

    def array: Backend.Exp = x match {
      case g.Def("tensor", Backend.Const(s:Seq[Int])::(array:Backend.Sym)::Nil) => array
      case a => System.out.println(a); ???
    }

    def + (y: Tensor): Tensor = {
      // compute result shape
      assert(shape == y.shape)
      Tensor(g.reflect("tensor_add", Backend.Const(shape), x, y.x))
    }


  }

  def Tensor(shape: Seq[Int], array: Backend.Exp): Tensor =
    Tensor(g.reflect("tensor", Backend.Const(shape), array))

  override def mkGraphBuilder() = new GraphBuilderOpt
}


abstract class TensorAdd extends Transformer {
  val frontEnd: FixedSizeTensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_add", List(Backend.Const(size:Seq[Int]), x: Backend.Sym, y:Backend.Sym), _) =>
      val array = g.reflect("array_add", Tensor(x).array, Tensor(y).array)
      Tensor(size, array).x
    case _ => super.transform(n)
  }
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
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
    Adapter.g = frontEnd.g
  }

  val tensor2array = new mutable.HashMap[Backend.Sym, Backend.Sym]

  def numeral(size: Seq[Int]) = size.foldLeft(1)(_ * _)

  // FIXME(feiw) what do we do about different types? Int Float?
  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::Nil, _) =>
      tensor2array(s) = transform(x).asInstanceOf[Backend.Sym]
      s
    case Node(s, "tensor_add", Backend.Const(size:Seq[Int])::(x:Backend.Sym)::(y:Backend.Sym)::Nil, _) =>
      // FIXME(feiw) should do match case on type Manifest?
      System.out.println(tensor2array)
      System.out.println(s"$x $y")
      val arr1 = Wrap[Array[Int]](tensor2array(x))
      val arr2 = Wrap[Array[Int]](tensor2array(y))
      val res = array_add[Int](arr1, arr2, numeral(size))
      tensor2array(s) = Unwrap(res).asInstanceOf[Backend.Sym]
      Unwrap(res)

    case Node(s, "tensor_show", List(x: Backend.Sym), _) => graphCache.get(x) match {
      case Some(Node(s, op, Backend.Const(size:Seq[Int])::_, _)) =>
        val arr = Wrap[Array[Int]](tensor2array(x))
        array_print[Int](arr, numeral(size))
        Backend.Const(())
      case a => System.out.println(a); ???
    }

    case _ => super.transform(n)

  }
}

abstract class TensorShow extends Transformer {
  // transformer will build a new graph from old graph. Needs two components:
  // 1. A graphBuilder g.
  // 2. If we do not want to directly use g.reflect, we need some FrontEnds, which also
  //    has a graphBuilder g.
  val frontEnd: FixedSizeTensorFrontEnd
  import frontEnd._
  def init() = {
    frontEnd.g = frontEnd.mkGraphBuilder()
    g = frontEnd.g
  }

  def forloop(size: Int)(f: INT => Unit) = {
    val loopVar = VAR(0)

    // fix adapter.typeMap
    Adapter.typeMap(loopVar.x) = manifest[Int]

    WHILE (loopVar() !== INT(Backend.Const(size))) {
      f(loopVar())
      loopVar() = loopVar() + 1
    }
  }

  def forloops(sizes: Seq[Int])(f: Seq[INT] => Unit): Unit = sizes match {
    case h +: tail => forloop(h) { i => forloops(tail) { is => f(i +: is) } }
    case _ => f(Seq())
  }

  def flatIndex(sizes: Seq[Int], index: Seq[INT]): INT = {
    assert(sizes.size == index.size)
    val (strides, offsets) = (sizes zip index).foldRight((1, INT(Backend.Const(0)))) {
      case ((s, i), (stride, offset)) =>
        val temp = i * stride
        Adapter.typeMap(temp.x) = manifest[Int]
        (s * stride, temp + offset)
    }
    offsets
  }

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor_show", List(x: Backend.Sym), _) => graphCache.get(x) match {
      case Some(Node(s, "tensor", Backend.Const(size:Seq[Int])::(arr:Backend.Exp)::Nil, _)) =>
        // print the array
        forloops(size) { index =>
          val findex = flatIndex(size, index)
          val ele = g.reflectRead("array_get", arr, findex.x)(arr)
          PRINT(INT(ele))
        }
        Backend.Const(())
      case a => System.out.println(a); ???
    }

    case _ => super.transform(n)
  }
}
