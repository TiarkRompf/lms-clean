package lms.collection.mutable

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.macros.RefinedManifest

trait StructOps extends Base with ArrayOps {
  abstract class Struct

  class Pointer[T <: Struct:RefinedManifest](val ptr: Rep[LongArray[T]], base: Rep[LongArray[T]]) {
    def readField[U: Manifest](field: String): Rep[U] =
      Wrap[U](Adapter.g.reflectRead("reffield_get", Unwrap(ptr), Unwrap(field))(Unwrap(base)))
    def writeField[U: Manifest](field: String, v: Rep[U]): Unit =
      Adapter.g.reflectWrite("reffield_set", Unwrap(ptr), Unwrap(field), Unwrap(v))(Unwrap(base))
    def deref: Rep[T] =
      Wrap[T](Adapter.g.reflectWrite("deref", Unwrap(ptr))(Adapter.CTRL))
  }

  object Pointer {
    def apply[T <: Struct:RefinedManifest](v: Rep[T]): Pointer[T] = Pointer(LongArray[T](v))
    def apply[T <: Struct:RefinedManifest](arr: Rep[LongArray[T]], idx: Rep[Long] = 0L) = arr match {
      case Wrap(_) => new Pointer(arr.slice(idx), arr)
      case EffectView(_, base) => new Pointer(arr.slice(idx), base)
    }
    def local[T <: Struct:RefinedManifest] = {
      val struct = Wrap[T](Adapter.g.reflectMutable("local_struct"))
      val ptr = Wrap[LongArray[T]](Adapter.g.reflectEffect("ref_new", Unwrap(struct))(Unwrap(struct), Adapter.STORE)()) // FIXME: Write? Or different?
      new Pointer[T](ptr, ptr)
    }
  }
}

trait CCodeGenStruct extends ExtendedCCodeGen {
  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "reffield_set", List(ptr, Const(field: String), v), _) =>
      shallowP(ptr, precedence("reffield_get"))
      esln"->$field = $v;"
    case n @ Node(s, "local_struct", Nil, _) =>
      val tpe = remap(typeMap.getOrElse(s, manifest[Unknown]))
      emitln(s"$tpe ${quote(s)} = { 0 };") // FIXME: uninitialized? or add it as argument?
    case _ => super.traverse(n)
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s, "ref_new", List(v), _) =>
      emit("&"); shallowP(v, precedence("ref_new"))
    case n @ Node(s, "reffield_get", List(ptr, Const(field: String)), _) =>
      shallowP(ptr, precedence("reffield_get")); emit("->"); emit(field)
    case n @ Node(s, "deref", List(ptr), _) =>
      es"*$ptr"
    case _ => super.shallow(n)
  }

  override def mayInline(n: Node): Boolean = n match {
    case _ => super.mayInline(n)
  }
}
