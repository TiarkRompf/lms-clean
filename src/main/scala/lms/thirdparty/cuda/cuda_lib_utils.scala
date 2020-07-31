package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.collection.mutable.ArrayOps

import lms.collection._

trait CudaFunction extends Base {
  def cudaFunction[T: Manifest](
      m: String,
      configs: Seq[lms.core.Backend.Exp],
      rhs: lms.core.Backend.Exp*
  )(rkeys: Seq[Int], wkeys: Seq[Int], pkeys: Set[Int], keys: lms.core.Backend.Const*): Rep[T] = {
    val readKeys = rkeys.map(rhs(_))
    val writeKeys = wkeys.map(rhs(_)) ++ keys
    val defs = Seq(
      lms.core.Backend.Const(m),
      lms.core.Backend.Const(configs.length),
      lms.core.Backend.Const(pkeys)
    ) ++ configs ++ rhs
    Wrap[T](Adapter.g.reflectEffect("lib-function", defs: _*)(readKeys: _*)(writeKeys: _*))
  }
}

trait CudaCodeGenLibFunction extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit =
    n match {
      case Node(s, "lib-function", Const(m: String) :: Const(c: Int) :: Const(pkeys: Set[Int]) :: defs, _) =>
        val configs = defs.take(c)
        val rhs = defs.drop(c)
        val last = rhs.length - 1
        emit(s"$m<<<"); shallow(configs.head);
        configs.tail.foreach(a => { emit(", "); shallow(a) });
        emit(">>>(");
        rhs.zipWithIndex.foreach {
          case (r, index) =>
            if (pkeys.contains(index)) emit("&")
            shallow(r)
            if (index < last) emit(", ")
        }
        emit(")")
      case _ => super.shallow(n)
    }
}
