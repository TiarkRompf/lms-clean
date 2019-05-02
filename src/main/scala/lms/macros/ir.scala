package lms.macros

//import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

import scala.util.matching.Regex

import scala.collection.mutable

/*
This macro implements the following translations:

  @ir def foo(x1: T1, x2: T2): Tn
  -------------------------------
  def foo(x1: T1, x2: T2): Tn = reflect[Tn]("foo",ref(x1),ref(x2))
  object Mfoo {
    def unapply(xx_x: Any): Option[(T1,T2)] = xx_x match {
      case Reflect("foo", List(x1,x2)) => Some((unref[T1](x1),unref[T2](x2)))
    }
  }

  @ir def foo(x1: T1, x2: T2): Tn = body
  --------------------------------------
  rewrite { case Mfoo(x1,x2) => foo_next(x1,x2) }
  def foo(x1: T1, x2: T2): Tn = ...
  def foo_next(x1: T1, x2: T2): Tn = body
  object Mfoo { ... }
*/


object ir {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    println("--- @ir macro ---")
    println(c.prefix)

    val target = c.prefix.tree match {
      case q"new $annot($arg)" => Some(arg)
      case _ => None
    }

    val List(a) = annottees
    println(a)
    def transform(a: c.Expr[Any]): c.Expr[Any] = a.tree match {

      // TODO
      // + 1. Proper reflectEffect call
      // + 2. Method annotation for allocation (e.g. TensorBuilder1)
      // - 3. Extract meaningful arg annotation symbol?

      case q"def $name[..$t](..$args): $tpe" =>
        val effects = args collect { case v@ValDef(_,x,t,y) if v.toString contains("@") => q"Write(ref($x).asInstanceOf[Sym])" } //XXX FIXME: currently any ann counts as effect!
        val args1 = args map { case ValDef(_,x,_,_) => q"ref($x)" }
        val tpes = args map { case ValDef(_,x,t,_) => q"$t" }
        val name_extract = TermName("M"+name.toString)
        val args3 = args map { case ValDef(_,x,_,_) => pq"$x" }
        val args2 = args map { case ValDef(_,x,t,_) => q"unref[$t]($x)" }
        return c.Expr(q"""
          def $name[..$t](..$args): $tpe = reflect[$tpe](${name.toString},..$args1)(..$effects)
          object $name_extract {
            def unapply(xx_x: Any): Option[(..$tpes)] = xx_x match {
              case Reflect(${Literal(Constant(name.toString))}, List(..$args3)) => Some((..$args2))
              case _ => None
            }
          }
        """)
      case q"def $name[..$t](..$args): $tpe = $body" =>
        // TODO: strip by-name type (?)
        var effects = args collect { case v@ValDef(_,x,t,y) if v.toString contains("@") => q"Write(ref($x).asInstanceOf[Sym])" } //XXX FIXME: any ann counts as effect!
        var effects2 = args collect { case v@ValDef(m,x,t,y) if m.annotations.nonEmpty => q"Write(ref($x).asInstanceOf[Sym])" }
        assert(effects.toString == effects2.toString, s"$effects != $effects2")
        if (tpe.toString.contains("@")) {
          println(tpe.getClass)
          tpe match {
            case tq"$t @..$foo" => println(s"YAY $t $foo")
            case _ => println("NAY")
          }
          //XXX TODO cleanup!!
          // println(tpe.asInstanceOf[scala.reflect.internal.Trees$Tree].annotations)
          println("XXXXXXXXXXXXX "+tpe)
          effects :+= q"Alloc" // q"Const(STORE)"
        }

        val args0 = args map { case ValDef(_,x,_,_) => q"$x" }
        val args1 = args map { case ValDef(_,x,_,_) => q"ref($x)" }
        val tpes = args map { case ValDef(_,x,t,_) => q"$t" }
        val name_extract = TermName("M"+name.toString)
        val name_next = TermName(name.toString + "_next")
        val args3 = args map { case ValDef(_,x,_,_) => pq"$x" }
        val args2 = args map { case ValDef(_,x,t,_) => q"unref[$t]($x)" }
        val lower = target match { case Some(t) => q"rewriteAt($t)" case _ => q"rewrite"}
        return c.Expr(q"""
          $lower { case $name_extract(..$args3) => $name_next(..$args0) }
          def $name[..$t](..$args): $tpe = reflect[$tpe](${name.toString},..$args1)(..$effects)
          def $name_next[..$t](..$args): $tpe = $body
          object $name_extract {
            def unapply(xx_x: Any): Option[(..$tpes)] = xx_x match {
              case Reflect(${Literal(Constant(name.toString))}, List(..$args3)) => Some((..$args2))
              case _ => None
            }
          }
        """)
     // TODO class def
      //case t@ClassDef(_,_,_,_) =>
    }
    transform(a)
  }
}
