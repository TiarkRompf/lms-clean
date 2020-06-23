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

// Use for libStruct Type Mapping
class CustomManifest[T: Manifest] extends Manifest[T] {
  def runtimeClass = manifest[T].runtimeClass
}

trait LibStruct { b : Base =>
  def registerCType[T:Manifest](m: String) = new CustomManifest[T] { override def toString = m }

  def newStruct[T:CustomManifest]: Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("lib-struct", lms.core.Backend.Const(manifest[T].toString)))
  }

  // T: type of struct . U: type of field
  def readField[T:Manifest, U:Manifest](x: Rep[T], m:String): Rep[U] = {
    Wrap[U](Adapter.g.reflectRead("read-field", Unwrap(x), lms.core.Backend.Const(m))(Unwrap(x)))
  }
}

trait CCodeGenLibStruct extends ExtendedCCodeGen {

  override def remap(m: Manifest[_]) = m match {
    case cus: CustomManifest[_] => m.toString
    case _ => super.remap(m)
  }

  override def mayInline(n: Node): Boolean = n match {
    case Node(_, "lib-struct", _, _) => false
    case _ => super.mayInline(n)
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "read-field", List(x: Sym, Const(m: String)), _) =>
      shallow(x); emit(s".$m")
    case _ => super.shallow(n)
  }

  override def traverse(n: Node): Unit = n match {
    case Node(s, "lib-struct", List(Const(m: String)), _) =>
      emit(s"$m "); shallow(s); emitln(";")
    case _ => super.traverse(n)
  }
}

trait LibFunction { b: Base =>
  def libFunction[T:Manifest](m:String, rhs:lms.core.Backend.Exp*)(rkeys:Seq[Int], wkeys:Seq[Int], pkeys:Set[Int], keys: lms.core.Backend.Const*): Rep[T] = {
    val readKeys = rkeys.map(rhs(_))
    val writeKeys = wkeys.map(rhs(_)) ++ keys
    val defs = Seq(lms.core.Backend.Const(m), lms.core.Backend.Const(pkeys)) ++ rhs
    Wrap[T](Adapter.g.reflectEffect("lib-function", defs:_*)(readKeys: _*)(writeKeys: _*))
  }
}

trait CCodeGenLibFunction extends ExtendedCCodeGen {
  override def shallow(n: Node): Unit = n match {
    case Node(s, "lib-function", Const(m:String) :: Const(pkeys: Set[Int]) :: rhs, _) =>
      val last = rhs.length - 1
      emit(s"$m(");
      rhs.zipWithIndex.foreach { case(r, index) =>
        if (pkeys.contains(index)) emit("&")
        shallow(r)
        if (index < last) emit(", ")
      }
      emit(")")
    case _ => super.shallow(n)
  }
}
