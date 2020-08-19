package lms
package tensors

import scala.annotation.implicitNotFound

import lms.core._
import lms.core.stub._

import Backend._

trait FixedSizeTensorFrontEnd extends FrontEndT with Base {

  case class Tensor(x: Backend.Exp) {
    def shape: Seq[Int] = x match {
      case g.Def("tensor", Backend.Const(s:Seq[Int])::_) => s
      case a => System.out.println(a); ???
    }

    def show: Rep[Unit] = Wrap[Unit](g.reflectEffect("tensor_show", x)()(Adapter.CTRL))

  }

  def Tensor(shape: Seq[Int], array: Backend.Exp): Tensor =
    Tensor(g.reflect("tensor", Backend.Const(shape), array))

  override def mkGraphBuilder() = new GraphBuilderOpt
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
    val (strides, offsets) = (sizes zip index).foldRight((1, INT(Backend.Const(0)))) { case ((s, i), (stride, offset)) =>
      (s * stride, i * stride + offset)
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
