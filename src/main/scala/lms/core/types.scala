package lms.core
package types

import lms.core
import lms.core.Backend.{Const, Exp, Sym}
import lms.core.stub.{Adapter, Base}
import lms.core.types.TypeConstructors.{FunT, InferredT}
import lms.core.types.TypeMap.TypeExp

import scala.collection.mutable


case class TypeMap(g: GraphBuilder) extends mutable.HashMap[Exp, TypeExp]

object TypeMap {

  type TypeExp = Exp

  def apply(): TypeMap = {
    // TODO:  should we have the same GraphBuilder for types
    //        and backend or can we refactor it
    //        to share some code?
    val g = new GraphBuilder
    // TODO:  why is `g` initialized without any `curLocalDefs`?
    //        What about `curLocalReads` etc which are not used yet.
    g.curLocalDefs = Set()
    TypeMap(g)
  }

}

abstract class TypeDef[T] {
  type Rep[T]

  def fromExp(x: Exp): Rep[T]

  def toExp(x: Rep[T]): Exp

  def reflect(tm: TypeMap): TypeExp
}

object TypeExp {
  def of[T](implicit t: TypeDef[T], typeMap: TypeMap): TypeExp = t.reflect(typeMap)
}

object TypeNames extends Enumeration {
  val InferredT, UnitT, BooleanT, CharT, ShortT, IntT, FloatT, DoubleT = Value
}

// Primitive types corresponding to Scala ones
object TypeConstructors {

  val InferredT: TypeExp = Const(TypeNames.InferredT)

  val UnitT: TypeExp = Const(TypeNames.UnitT)
  val BooleanT: TypeExp = Const(TypeNames.BooleanT)
  val CharT: TypeExp = Const(TypeNames.CharT)
  val ShortT: TypeExp = Const(TypeNames.ShortT)
  val IntT: TypeExp = Const(TypeNames.IntT)
  val FloatT: TypeExp = Const(TypeNames.FloatT)
  val DoubleT: TypeExp = Const(TypeNames.DoubleT)


  object ArrayT {
    val name = "ArrayT"

    def apply(et: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(ArrayT.name, et)

    def unapply(x: Any)(implicit tm: TypeMap): Option[TypeExp] = x match {
      case tm.g.Def(ArrayT.name, (t: TypeExp) :: Nil) => Some(t)
    }
  }

  object FunT {
    val name = "FunT"

    def apply(it: List[TypeExp], rt: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(FunT.name, Const(it), rt)

    def unapply(x: Any)(implicit tm: TypeMap): Option[(List[TypeExp], TypeExp)] = x match {
      case tm.g.Def(FunT.name, Const(its: List[TypeExp@unchecked]) :: (rt: TypeExp) :: Nil) => Some((its, rt))
    }

  }

}

// TODO move to stub?
trait ImplicitScalaTypes {
  b: Base =>

  import TypeConstructors._

  abstract class ScalaType[T] extends TypeDef[T] {
    override type Rep[T] = Exp[T]

    override def fromExp(x: core.Backend.Exp): Rep[T] = ??? // Wrap[T](x)
    override def toExp(x: Rep[T]): core.Backend.Exp = Unwrap(x)
  }

  // Scala base types for compatibility with `Rep[_]` frontend
  implicit val mkUnitType: ScalaType[Unit] = new ScalaType[Unit] {
    override def reflect(tm: TypeMap): TypeExp = UnitT
  }
  implicit val mkBooleanType: ScalaType[Boolean] = new ScalaType[Boolean] {
    override def reflect(tm: TypeMap): TypeExp = BooleanT
  }
  implicit val mkCharType: ScalaType[Char] = new ScalaType[Char] {
    override def reflect(tm: TypeMap): TypeExp = CharT
  }
  implicit val mkShortType: ScalaType[Short] = new ScalaType[Short] {
    override def reflect(tm: TypeMap): TypeExp = ShortT
  }
  implicit val mkIntType: ScalaType[Int] = new ScalaType[Int] {
    override def reflect(tm: TypeMap): TypeExp = IntT
  }
  implicit val mkFloatType: ScalaType[Float] = new ScalaType[Float] {
    override def reflect(tm: TypeMap): TypeExp = FloatT
  }
  implicit val mkDoubleType: ScalaType[Double] = new ScalaType[Double] {
    override def reflect(tm: TypeMap): TypeExp = DoubleT
  }

  implicit def mkArrayType[T](implicit te: ScalaType[T]): ScalaType[Array[T]] = new ScalaType[Array[T]] {
    override def reflect(tm: TypeMap): TypeExp = ArrayT(te.reflect(tm))(tm)
  }

}


trait Types {
  implicit val tm: TypeMap

  abstract class Type[T] extends TypeDef[T] {
    override type Rep[T] = T

    override def fromExp(x: Exp): T

    override def toExp(x: Rep[T]): Exp
  }


  def ref[T: Type](x: T): Exp = implicitly[Type[T]].toExp(x)

  def unref[T](x: Exp)(implicit t: Type[T]): T = {
    tm.getOrElseUpdate(x, t.reflect(tm))
    t.fromExp(x)
  }

  implicit def mkFunType[T, R](implicit it: Type[T], rt: Type[R]): Type[T => R] = new Type[T => R] {
    override def reflect(tm: TypeMap): TypeExp = FunT(it.reflect(tm) :: Nil, rt.reflect(tm))(tm)
    override def fromExp(x: Exp): T => R = (nd: T) => unref[R](Adapter.g.reflectEffect("@", x, ref(nd))()())
    override def toExp(f: T => R): Exp = Adapter.g.reflect("λ", Adapter.g.reify(xn => ref(f(unref[T](xn)))))
  }

  implicit def mkFunType2[T1, T2, R](implicit it1: Type[T1], it2: Type[T2], rt: Type[R]): Type[(T1, T2) => R] = new Type[(T1, T2) => R] {
    override def reflect(tm: TypeMap): TypeExp = FunT(List(it1.reflect(tm), it2.reflect(tm)), rt.reflect(tm))(tm)
    override def fromExp(x: Exp): (T1, T2) => R = (t1: T1, t2: T2) => unref[R](Adapter.g.reflectEffect("@", x, ref(t1), ref(t2))()())
    override def toExp(f: (T1, T2) => R): Exp = Adapter.g.reflect("λ", Adapter.g.reify((x1, x2) => ref(f(unref[T1](x1), unref[T2](x2)))))
  }
}


trait ExperimentalBase {
  t: Types =>

  def fun[T, R](f: T => R)(implicit t: Type[T], r: Type[R]): T => R = fun(t, r)(f)

  def fun[T1: Type, T2: Type, R: Type](f: (T1, T2) => R)
                                      (implicit t1: Type[T1], t2: Type[T2], r: Type[R]): (T1, T2) => R = fun((t1, t2), r)(f)

  def fun[T, R](it: Type[T], rt: Type[R])(f: T => R): T => R = {
    val t = mkFunType(it, rt)
    unref(ref(f)(t))(t)
  }

  def fun[T1, T2, R](it: (Type[T1], Type[T2]), rt: Type[R])(f: (T1, T2) => R): (T1, T2) => R = {
    val t = mkFunType2(it._1, it._2, rt)
    unref(ref(f)(t))(t)
  }


  // TODO: `fix` functions for recursive definitions maybe ?
  def fix(arity: Int)(f: (List[Exp] => Exp, List[Exp]) => Exp): Exp = {
    val fn = Sym(Adapter.g.fresh)

    val f1 = (xs: List[Exp]) => Adapter.g.reflect("@", fn +: xs: _*)

    Adapter.g.reflect(fn, "λ", Adapter.g.reify(arity, xn => f(f1, xn)))()
  }

  def fix[T, R](it: Type[T], rt: Type[R])(f: (T => R, T) => R): T => R = {
    val fn = Sym(Adapter.g.fresh)
    tm(fn) = mkFunType(it, rt).reflect(tm)

    val f1 = (x: T) => app(fn, x)(it, rt)

    Adapter.g.reflect(fn, "λ", Adapter.g.reify(xn => ref(f(f1, unref(xn)(it)))(rt)))()

    f1
  }

  private def app[T: Type, R: Type](f: Exp, x: T): R = unref[R](Adapter.g.reflect("@", f, ref(x)))

  trait Numeric[T] {
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T
    def times(x: T, y: T): T
    def negate(x: T): T
  }

  implicit class NumericOps[T](lhs: T)(implicit num: Numeric[T]) {
    def +(rhs: T): T = num.plus(lhs, rhs)
    def -(rhs: T): T = num.minus(lhs, rhs)
    def *(rhs: T): T = num.times(lhs, rhs)
    def infix_- : T = num.negate(lhs)
  }

  abstract class GenericNumeric[T:Type] extends Numeric[T] {
    override def plus(x: T, y: T): T = unref[T](Adapter.g.reflect("+", ref(x), ref(y)))
    override def minus(x: T, y: T): T = unref[T](Adapter.g.reflect("-", ref(x), ref(y)))
    override def times(x: T, y: T): T = unref[T](Adapter.g.reflect("*", ref(x), ref(y)))
    override def negate(x: T): T = unref[T](Adapter.g.reflect("-", ref(x)))
  }

  case class i64(x: Exp)

  implicit object i64IsType extends Type[i64] {
    override def fromExp(x: Exp): i64 = i64(x)
    override def toExp(x: i64): Exp = x.x
    override def reflect(tm: TypeMap): TypeExp = Const("i64")
  }

  implicit object i64IsNumeric extends GenericNumeric[i64]

}

// Types and IR nodes defined outside of LMS core
package object tensor {

  trait TensorOps {
    _: Types with ExperimentalBase =>

    object TensorT {
      val name = "TensorT"

      def apply(et: TypeExp, shape: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(TensorT.name, et, shape)

      def unapply(x: Any)(implicit tm: TypeMap): Option[(TypeExp, List[Range])] = x match {
        case tm.g.Def(TensorT.name, (t: TypeExp) :: Const(shape: List[Range @unchecked]) :: Nil) => Some((t, shape))
      }
    }

    case class Tensor[T: Type](x: Exp) {
      def apply(i: i64): Tensor[T] = unref[Tensor[T]](Adapter.g.reflect("slice", x, i.x))(t[T](Range(InferredT)))
      def apply(i: i64, j: i64): Tensor[T] = unref[Tensor[T]](Adapter.g.reflect("slice", x, i.x, j.x))(t[T](Range(InferredT)))
      def v: T = unref[T](Adapter.g.reflect("get", x))
    }

    case class Range(x: Exp)

    object freshRange {
      var idx = -1
      def apply() = Range(Const("?i" + { idx += 1; idx }))
    }

    def ? = freshRange()

    def t[T: Type](shape: Range*): Type[Tensor[T]] = new Type[Tensor[T]] {
      override def reflect(tm: TypeMap): TypeExp = TensorT(implicitly[Type[T]].reflect(tm), Const(shape.toList))(tm)

      override def fromExp(x: Exp): Tensor[T] = Tensor[T](x)

      override def toExp(x: Tensor[T]): Exp = x.x
    }

    def `for`[T:Type](i: Range)(body: i64 => T): Tensor[T] = unref[Tensor[T]](Adapter.g.reflect("for", i.x, ref(body)))(t[T](Range(InferredT)))

    def `for`[T:Type](i: Range, j: Range)(body: (i64, i64) => T): Tensor[T] = unref[Tensor[T]](Adapter.g.reflect("for", i.x, j.x, ref(body)))(t[T](Range(InferredT)))

    def sum[T:Type](x: Tensor[T]): T = unref[T](Adapter.g.reflect("sum", ref(x)(t[T](Range(InferredT)))))

    def matMul[T:Numeric:Type](t1: Tensor[T], t2: Tensor[T], n: Range = ?, k: Range = ?, m: Range = ?): Tensor[T] = {
      fun(it=(t[T](n, k), t[T](k, m)), rt=t[T](n, m))((t1, t2) => {
        `for`(n, m) { (n, m) =>
          sum {
            `for`(k) { k => t1(n, k).v * t2(k, m).v }
          }
        }
      })(t1, t2)
    }
  }

}

object Main {


  def main(args: Array[String]): Unit = {
    Adapter.resetState

    val ops = new ExperimentalBase with tensor.TensorOps with Types {
      override implicit val tm: TypeMap = TypeMap()
    }

    import ops._

    val boh = Adapter.program(Adapter.g.reify((x1, x2) => matMul(new Tensor[i64](x1), new Tensor[i64](x2)).x))
    println(boh)
    println(tm)
    println(tm.g.globalDefs)


    //    val s1 = Sym(1)
    //    val s2 = Sym(2)
    //    val s3 = tm.g.reflect("Tensor", Const(/* shape = */ List(130, 20)))
    //
    //    val foo = new ImplicitScalaTypes with Base {}
    //    import foo._
    //
    //    Wrap[Array[Array[Float]]](s1)
    //    Wrap[Array[Array[Int]]](s2)
    ////    Wrap[Tensor[Float]](s3)
    //
    //    tm(s1) match {
    //      case ArrayT(ArrayT(t)) => println(s"element type $t")
    //    }
    //
    //    tm(s2) match {
    //      case ArrayT(ArrayT(IntT)) => println("is Int")
    //    }
    //
    //    tm(s3) match {
    //      case TensorT(et, shape) => println(s"TensorT($et, $shape) where shape[0]=${shape.head}")
    //    }
    //
    //    println(tm)
    //    println(tm.g.globalDefsCache)
  }

}
