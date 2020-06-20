package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

import lms.collection._


trait CMacro { b: Base =>
  def cmacro[T:Manifest](m:String): Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("macro", lms.core.Backend.Const(m)))
  }
}
trait CCodeGenCMacro extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(_, "macro", List(Const(m:String)), _) =>
      emit(m)
    case _ => super.shallow(n)
  }
}
