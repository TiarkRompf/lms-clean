package lms.thirdparty

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext

trait SizeTOps extends Base with CLibs {

  // abstract class SizeT
  // NOTE(feiw) as a short-cut for constant int :)
  case class SizeT(x: Int) { override def toString() = x.toString }
  implicit def sizeTFromInt(x: Int) = SizeT(x)

  // NOTE(feiw) as a normal Rep[SizeT] construction from Rep[Int]
  def SizeT(x: Rep[Int]): Rep[SizeT] = {
    Wrap[SizeT](Adapter.g.reflect("SizeT", Unwrap(x)))
  }

  // we offer SizeT to Int type casting as a method
  implicit def sizeTRepToOps(x: Rep[SizeT])(implicit __pos: SourceContext): SizeTOps = new SizeTOps(x)(__pos)
  implicit def sizeTVarToOps(x: Var[SizeT])(implicit __pos: SourceContext): SizeTOps = new SizeTOps(readVar(x))(__pos)
  class SizeTOps(x: Rep[SizeT])(implicit __pos: SourceContext) {
    def toInt: Rep[Int] = Unwrap(x) match {
      case Adapter.g.Def("SizeT", List(int_x: Backend.Exp)) => Wrap[Int](int_x)
      case _ => ???
    }
  }
}

trait CCodeGenSizeTOps extends ExtendedCCodeGen {
  override def remap(m: Manifest[_]): String = m.runtimeClass.getName match {
    case s: String if s.endsWith("SizeT") => "size_t"
    case _ => super.remap(m)
  }
}
