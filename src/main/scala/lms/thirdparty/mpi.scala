package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

trait MPIOps { b: Base =>
  /* LMS support for MPI library */
  def mutate(x: Rep[Int]): Rep[Unit] = {
    Wrap[Unit](Adapter.g.reflectWrite("macro-mutate", Unwrap(x))(Unwrap(x)))
  }
}

trait CCodeGenMPI extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(s, "macro-mutate", List(x), _) =>
      emit("MUTATE("); shallow(x); emit(")")
    case _ => super.shallow(n)
  }
}