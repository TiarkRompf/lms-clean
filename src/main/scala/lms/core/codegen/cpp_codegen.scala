package lms.core

import lms.macros.RefinedManifest

import scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}

import Backend._
import lms.core.stub._

trait CPPOps { b: Base =>

  abstract class Capture
  object Capture {
    def apply(s: String) = Seq(CapAll(s))
    def CopyExcept[T: Manifest](s: Rep[T]*) = Seq(CapAll("=")) ++ s.map(RefOne(_))
    def RefExcept[T: Manifest](s: Rep[T]*) = Seq(CapAll("&")) ++ s.map(CopyOne(_))
  }
  case class CapAll(s: String) extends Capture
  case class CopyOne[T: Manifest](s: Rep[T]) extends Capture
  case class RefOne[T: Manifest](s: Rep[T]) extends Capture
  implicit def liftCapture(s:String) = Capture(s)
  private def unwrap(c: Capture) = c match {
    case CapAll(s: String) => lms.core.Backend.Const(s)
    case CopyOne(s: Rep[_]) => Unwrap(s)
    case RefOne(s: Rep[_]) => Unwrap(s)
  }

  def fun[A:Manifest,B:Manifest](capture: Seq[Capture], f: Rep[A] => Rep[B]): Rep[A => B] =
    Wrap[A=>B](__fun(f, 1, xn => Unwrap(f(Wrap[A](xn(0)))), capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest](capture: Seq[Capture], f: (Rep[A], Rep[B]) => Rep[C]): Rep[(A, B) => C] =
    Wrap[(A,B)=>C](__fun(f, 2, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)))), capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest](capture: Seq[Capture], f: (Rep[A], Rep[B], Rep[C]) => Rep[D]): Rep[(A, B, C) => D] =
    Wrap[(A,B,C)=>D](__fun(f, 3, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)))), capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](capture: Seq[Capture], f: (Rep[A], Rep[B], Rep[C], Rep[D]) => Rep[E]): Rep[(A, B, C, D) => E] =
    Wrap[(A,B,C,D)=>E](__fun(f, 4, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)))), capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest](capture: Seq[Capture], f:(Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]) => Rep[F]):
    Rep[(A, B, C, D, E) => F] =
    Wrap[(A,B,C,D,E)=>F](__fun(f, 5, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)))), capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest]
    (capture: Seq[Capture], f:(Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F]) => Rep[G]): Rep[(A, B, C, D, E, F) => G] =
    Wrap[(A,B,C,D,E,F)=>G](__fun(f, 6, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)))), capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest]
    (capture: Seq[Capture], f:(Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G]) => Rep[H]): Rep[(A, B, C, D, E, F, G) => H] =
    Wrap[(A,B,C,D,E,F,G)=>H](__fun(f, 7, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)))),
                                   capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest]
    (capture: Seq[Capture], f:(Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H]) => Rep[I]): Rep[(A, B, C, D, E, F, G, H) => I] =
    Wrap[(A,B,C,D,E,F,G,H)=>I](__fun(f, 8, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)))),
                                   capture.map(unwrap): _*))

  def fun[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest,F:Manifest,G:Manifest,H:Manifest,I:Manifest,J:Manifest]
    (capture: Seq[Capture], f:(Rep[A], Rep[B], Rep[C], Rep[D], Rep[E], Rep[F], Rep[G], Rep[H], Rep[I]) => Rep[J]): Rep[(A, B, C, D, E, F, G, H, I) => J] =
    Wrap[(A,B,C,D,E,F,G,H,I)=>J](__fun(f, 9, xn => Unwrap(f(Wrap[A](xn(0)), Wrap[B](xn(1)), Wrap[C](xn(2)), Wrap[D](xn(3)), Wrap[E](xn(4)), Wrap[F](xn(5)), Wrap[G](xn(6)), Wrap[H](xn(7)), Wrap[I](xn(8)))),
                                   capture.map(unwrap): _*))
}

trait ExtendedCPPCodeGen extends ExtendedCCodeGen {

  // remap for function types
  override def function(sig: List[Manifest[_]]): String = {
      registerHeader("<functional>")
      val ret = remap(sig.last)
      val params = sig.init.map(remap(_)).mkString(", ")
      s"std::function<$ret($params)>"
  }

  // inlining closure sometimes have trouble
  // for details, check test `returned_lambda_0`
  override def mayInline(n: Node): Boolean = n match {
    case Node(s, "λ", _, _) => false
    case _ => super.mayInline(n)
  }

  // emit closures with explicit argument types and return type
  // `argMod`/`retMod` specifies pass by copy or reference
  def quoteTypedBlock(b: Block, autoArgType: Boolean, retType: Boolean,
                      capture: String = "&", argMod: Option[List[String]] = None, retMod: Option[String] = None): Unit = {
    val eff = quoteEff(b.ein)
    val args = argMod match {
      case Some(mods) =>
        assert(mods.length == b.in.length, s"argMod should have same length as block inputs ${mods.length} ${b.in.length}")
        b.in.zipWithIndex.map{
          case (s, i) =>
            val mod = mods(i)
            if (autoArgType) s"auto$mod ${quote(s)}"
            else s"${remap(typeMap(s))}$mod ${quote(s)}"
        }.mkString(", ")
      case None => b.in.map{s =>
          if (autoArgType) s"auto ${quote(s)}"
          else s"${remap(typeMap(s))} ${quote(s)}"
      }.mkString(", ")
    }
    val mod = retMod.getOrElse("")
    val ret: String = if (retType) "->"+remap(typeBlockRes(b.res))+mod else ""

    // special case if capture is "mutable"
    if (capture == "mutable") emit(s"[=](${args}) mutable $ret $eff")
    else emit(s"[$capture](${args})$ret $eff")

    quoteBlockPReturn(traverse(b))
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s, "λ", (block: Block)::Nil, _) =>
      quoteTypedBlock(block, false, false, capture = "")
    case n @ Node(s, "λ", (block: Block)::Backend.Const(m:String)::rest, _) =>
      val cap = m match {
        case "=" => ("=" +: rest.map(r => s"&${quote(r)}")).mkString(", ")
        case "&" => ("&" +: rest.map(quote(_))).mkString(", ")
        case _ => ???
      }
      quoteTypedBlock(block, false, false, capture = cap)
    case _ => super.shallow(n)
  }

}
