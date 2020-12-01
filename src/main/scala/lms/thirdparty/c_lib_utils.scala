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


object CLibTypeLess {
  import BaseTypeLess._

  // FIXME(feiw) this CMACRO needs explicit cast to return non-TOP
  def CMACRO(s: String, m: Manifest[_])(implicit __pos: SourceContext): TOP = {
    TOP(Adapter.g.reflectUnsafe("macro", lms.core.Backend.Const(s)), m)
  }

  // FIXME(feiw) this NEW_STRUCT needs explicit cast to return non-TOP
  def NEW_STRUCT(m: Manifest[_], typeName: String)(implicit __pos: SourceContext): TOP = {
    TOP(Adapter.g.reflectUnsafe("lib-struct", Backend.Const(typeName)), m)
  }

  // FIXME(feiw) this LIB_FUNCTION needs explicit cast to return non-TOP
  def LIB_FUNCTION(manifest: Manifest[_], m: String, rhs: lms.core.Backend.Exp*)(rkeys:Seq[Int], wkeys: Seq[Int], pkeys: Set[Int], keys: lms.core.Backend.Exp*)(implicit __pos: SourceContext): TOP = {
    val readKeys = rkeys.map(rhs(_))
    val writeKeys = wkeys.map(rhs(_)) ++ keys
    val defs = Seq(lms.core.Backend.Const(m), lms.core.Backend.Const(pkeys)) ++ rhs
    TOP(Adapter.g.reflectEffect("lib-function", defs:_*)(readKeys: _*)(writeKeys: _*), manifest)
  }
}


trait CMacro extends Base {
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

trait LibStruct extends Base {
  def newStruct[T: Manifest](typeName: String): Rep[T] = {
    Wrap[T](Adapter.g.reflectUnsafe("lib-struct", Backend.Const(typeName)))
  }

  // T: type of struct . U: type of field
  def readField[T: Manifest, U: Manifest](x: Rep[T], m: String): Rep[U] = {
    Wrap[U](Adapter.g.reflectRead("read-field", Unwrap(x), Backend.Const(m))(Unwrap(x)))
  }
}

trait CCodeGenLibStruct extends ExtendedCCodeGen {
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
      esln"$m $s;"
    // allow "lib-struct" to take parameters
    case Node(s, "lib-struct", Const(m: String)::args, _) =>
      emit(s"$m "); shallow(s);
      emit("("); shallow(args.head);
      args.tail.foreach(a => {emit(", "); shallow(a)})
      emitln(");")
    case _ => super.traverse(n)
  }
}

trait LibFunction extends Base {
  def libFunction[T:Manifest](m:String, rhs:lms.core.Backend.Exp*)(rkeys:Seq[Int], wkeys:Seq[Int], pkeys:Set[Int], keys: lms.core.Backend.Exp*): Rep[T] = {
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

trait CLibs extends Base with CMacro with LibStruct with LibFunction
trait CCodeGenLibs extends CCodeGenCMacro with CCodeGenLibStruct with CCodeGenLibFunction
