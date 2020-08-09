package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.collection.immutable.{SeqOps, CCodeGenSeqOps}
import scala.io.Source

trait TensorOps1 extends Dsl {

  type Dim = Int
  class Tensor[T:Numeric:Manifest]

  object Tensor {
    def apply[N:Numeric:Manifest](arr: Rep[Array[N]], shape: Seq[Dim])(implicit pos: SourceContext): Rep[Tensor[N]] = {
      Wrap[Tensor[N]](Adapter.g.reflect("tensor-new", Backend.Const(shape), Unwrap(arr)))
    }
    def apply[N:Numeric:Manifest](arr: Rep[Array[N]], shape: Dim*): Rep[Tensor[N]] = apply(arr, shape)
  }

  implicit def __liftVarTensor[N:Numeric:Manifest](ts: Var[Tensor[N]]): TensorOpsCls1[N] = new TensorOpsCls1(readVar(ts))

  implicit class TensorOpsCls1[N:Numeric:Manifest](ts: Rep[Tensor[N]]) {
    // def shape: Rep[Seq[Int]] = Wrap[Seq[Int]](Adapter.g.reflect("tensor-shape", Unwrap(ts)))
    def shape: Seq[Dim] = ts match {
      case Adapter.g.Def("tensor-new", Backend.Const(sh:Seq[Dim])::_) => sh
      case _ => ???
    }
    // def apply(index: Rep[Seq[Int]]): Rep[N] = Wrap[N](Adapter.g.reflect("tensor-apply", Unwrap(ts), Unwrap(index)))
    // def apply(index: Rep[Int]*): Rep[N] = apply(Seq(index:_*))
    def show(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectEffect("tensor-show", Unwrap(ts))()(Adapter.CTRL))

    def +(y: Rep[Tensor[N]]): Rep[Tensor[N]] = Wrap[Tensor[N]](Adapter.g.reflect("tensor-add", Unwrap(ts), Unwrap(y)))
  }

}

// Now we want to add LMS IR transformation passes here to handle pre-codegen lowering.
class TensorLowering1 extends AdapterTransformer with TensorOps1 {

  override def transform(n: Node): Backend.Exp = n match {
    case Node(s, "tensor-new", List(arr:Backend.Sym, shape), _) =>
      subst(s) = arr
      ???
    case Node(s, "tensor-show", List(ts:Backend.Sym), _) => graphCache(ts) match {
      case Node(s, "tensor-new", List(Backend.Const(shape:Seq[Dim]), arr:Backend.Sym), _) =>
        val t = typeMap(arr)
        Unwrap(print_tensor(shape, arr))
      case _ =>
        System.out.println(graphCache(ts))
        ???
    }
    case _ => super.transform(n)
  }

  def print_tensor(shape: Seq[Dim], arr: Backend.Exp, offset: Rep[Dim] = 0): Rep[Unit] = shape match {
    case Seq() =>
      val m = typeMap.getOrElse(arr, manifest[Unknown])
      m.typeArguments.head.toString match {
        case s: String if s.endsWith("Float") =>
          val arr0 = Wrap[Array[Float]](arr)
          printf("%f ", arr0(offset))
        case s: String if s.endsWith("Int") => "%d "
          val arr0 = Wrap[Array[Int]](arr)
          printf("%d ", arr0(offset))
      }
    case d +: sh =>
      for (i <- (0 until d): Rep[Range]) {
        val off = offset + i * (if (sh.isEmpty) 1 else sh.foldLeft(1)(_*_))
        print_tensor(sh, arr, off)
      }
  }
}

trait CCodeGenTensorOps1 extends CCodeGenSeqOps {

}
