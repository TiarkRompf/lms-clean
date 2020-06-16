package lms.collection

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

trait PointerOps { b: Base =>
  object Pointer {
    def apply[A: Manifest](x: Rep[A])(implicit pos: SourceContext) = {
      Wrap[Pointer[A]](Adapter.g.reflect("pointer-new", Unwrap(x)))
    }
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
      emit("&")
      shallow(x)
    case _ => super.shallow(n)
  }
}