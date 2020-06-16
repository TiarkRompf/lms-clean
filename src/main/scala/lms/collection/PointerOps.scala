package lms.collection

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import scala.collection.mutable.HashMap

trait PointerOps { b: Base =>
  object Pointer {
    def apply[A: Manifest](x: Var[A])(implicit pos: SourceContext) = {
      val a = Wrap[Pointer[A]](Adapter.g.reflect("pointer-new", UnwrapV(x)))
      cache += ((Unwrap(a), UnwrapV(x)))
      a
    }
    def deref[A: Manifest](x: Rep[Pointer[A]]) = {
      Wrap[A](cache.getOrElse(Unwrap(x), ???))
    }
    private[this] val cache = HashMap[lms.core.Backend.Exp, lms.core.Backend.Exp]()
  }

  abstract class Pointer[T] extends Manifest[T]
}

trait CCodeGenPointer extends ExtendedCCodeGen {
  override def remap(m: Manifest[_]): String = {
    if (m.runtimeClass.getName == "lms.collection.PointerOps$Pointer") {
      val List(inner) = m.typeArguments
      s"${super.remap(inner)} *"
    } else { super.remap(m) }
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "pointer-new", List(x: Sym), _) =>
      emit("&"); shallow(x)
    case _ => super.shallow(n)
  }
}
