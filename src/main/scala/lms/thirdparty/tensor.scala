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

trait RandOps extends Base with CLibs {
  def randInt: Rep[Int] = cmacro[Int]("rand()")
  def rand[N:Numeric:Manifest]: Rep[N] = {
    Wrap[N](Adapter.g.reflect("cast", Unwrap(randInt), Backend.Const(manifest[N])))
  }
}

trait TensorOps extends Base with SeqOps with RandOps {

  type Dim = Int

  object Tensor {
    def apply[N:Numeric:Manifest](arr: Rep[Array[N]], shape: Seq[Dim])(implicit pos: SourceContext): Rep[Tensor[N]] = {
      Wrap[Tensor[N]](Adapter.g.reflect("tensor-new", Backend.Const(shape), Unwrap(arr)))
    }
    // def apply[N:Numeric:Manifest](arr: Rep[Array[N]], shape: Dim*): Rep[Tensor[N]] = apply(arr, shape)

    // def apply[N:Numeric:Manifest](shape: Seq[Dim])(f: Seq[Dim] => Rep[N])(implicit pos: SourceContext): Rep[Tensor[N]] = {
    //   Wrap[Tensor[N]](Adapter.g.reflect("tensor", Const(shape),
    //     Adapter.g.reify(xn => Unwrap(f(Wrap[Seq[Int]](xn))))))
    // }
    // def apply[N:Numeric:Manifest](shape: Rep[Int]*)(f: Rep[Seq[Int]] => Rep[N])(implicit pos: SourceContext): Rep[Tensor[N]] = {
    //   apply(Seq(shape:_*))(f)
    // }

    // def zeros[N:Numeric:Manifest](sh: Rep[Seq[Int]]): Rep[Tensor[N]] = {
    //   apply(sh)(xn => unit(implicitly[Numeric[N]].zero))
    // }
    // def zeros[N:Numeric:Manifest](sh: Rep[Int]*): Rep[Tensor[N]] = zeros(Seq(sh:_*))

    // def ones[N:Numeric:Manifest](sh: Rep[Seq[Int]]): Rep[Tensor[N]] = {
    //   apply(sh)(xn => unit(implicitly[Numeric[N]].one))
    // }
    // def ones[N:Numeric:Manifest](sh: Rep[Int]*): Rep[Tensor[N]] = ones(Seq(sh:_*))

    // def rands[N:Numeric:Manifest](sh: Rep[Seq[Int]]): Rep[Tensor[N]] = apply(sh)(xn => rand)
    // def rands[N:Numeric:Manifest](sh: Rep[Int]*): Rep[Tensor[N]] = rands(Seq(sh: _*))
  }

  class Tensor[T:Numeric:Manifest]

  implicit def __liftVarTensor[N:Numeric:Manifest](ts: Var[Tensor[N]]): TensorOpsCls[N] = new TensorOpsCls(readVar(ts))

  implicit class TensorOpsCls[N:Numeric:Manifest](ts: Rep[Tensor[N]]) {
    // def shape: Rep[Seq[Int]] = Wrap[Seq[Int]](Adapter.g.reflect("tensor-shape", Unwrap(ts)))
    def shape: Seq[Dim] = ts match {
      case Adapter.g.Def("tensor-new", Backend.Const(sh:Seq[Dim])::_) => sh
      case _ => ???
    }
    // def apply(index: Rep[Seq[Int]]): Rep[N] = Wrap[N](Adapter.g.reflect("tensor-apply", Unwrap(ts), Unwrap(index)))
    // def apply(index: Rep[Int]*): Rep[N] = apply(Seq(index:_*))
    def show(): Rep[Unit] = Wrap[Unit](Adapter.g.reflectEffect("tensor-show", Unwrap(ts))()(Adapter.CTRL))
    // def show(): Rep[Unit] = ts match {
    //   case Adapter.g.Def("tensor-new", Const(sh:Seq[Dim])::(arr:Sym)::Nil) =>
    //     for (i <- 0 )
    // }

    def +(y: Rep[Tensor[N]]): Rep[Tensor[N]] = Wrap[Tensor[N]](Adapter.g.reflect("tensor-add", Unwrap(ts), Unwrap(y)))
  }

}

// Now we want to add LMS IR transformation passes here to handle pre-codegen lowering.
abstract class TensorLowering extends Transformer {
  // when do we init g?
  // override def transform(n: Node): Exp = n match {
  //   case Node(s, "tensor-new", List(arr:Backend.Sym, shape), _) =>
  //     subst(s) = arr
  //     ???
  //   case Node(s, "tensor-show", List(ts:Backend.Sym), _) => ts match {
  //     case Adapter.g.Def("tensor-new", List(arr:Backend.Sym, shape)) => printTensor(shape, Wrap[Array[N]](arr))
  //     case _ => ???
  //   }
  // }

  type Dim = Int
  // def printTensor[N:Numeric:Manifest](shape: Seq[Dim], arr: Rep[Array[N]], offset: Dim = 0): Rep[Unit] = shape match {
  //   case Seq() => printf("%d ", arr(offset))
  //   case d +: sh =>
  //     for (i <- (0 until d: Rep[Range])) {
  //       printTensor(sh, arr, offset + i * sh.reduce(_ * _))
  //     }
  // }
}


trait CCodeGenTensorOps extends CCodeGenSeqOps {

}
