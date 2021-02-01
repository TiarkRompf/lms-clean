package lms.core

import lms.core.Backend.{Const, Exp, Sym}
import lms.core.TypeMap.TypeExp

import scala.collection.mutable


object Reflect {
  def unapply(x: Any)(implicit tm: TypeMap): Option[(String, List[Exp])] = x match {
    case s@Sym(_) =>
      tm.g.findDefinition(s).map(n => (n.op, n.rhs.filter(_.isInstanceOf[Exp]).map(_.asInstanceOf[Exp])))
    case _ => None
  }
}

abstract class Type[T] {
  def reify(x: Exp, tm: TypeMap): TypeExp
}

abstract class SimpleType[T] extends Type[T] {
  def reify(x: Exp, tm: TypeMap): TypeExp = reify(tm)
  def reify(tm: TypeMap): TypeExp
}

object TypeExp {
  def of[T](x: Exp)(implicit t: Type[T], typeMap: TypeMap): TypeExp = t.reify(x, typeMap)
}

object TypeNames extends Enumeration {
  // TODO maybe Int?
  val Unit, Boolean, Char, Short, Int, Float, Double = Value
}

object TypeConstants {
  val Unit:     TypeExp = Const(TypeNames.Unit)
  val Boolean:  TypeExp = Const(TypeNames.Boolean)
  val Char:     TypeExp = Const(TypeNames.Char)
  val Short:    TypeExp = Const(TypeNames.Short)
  val Int:      TypeExp = Const(TypeNames.Int)
  val Float:    TypeExp = Const(TypeNames.Float)
  val Double:   TypeExp = Const(TypeNames.Double)
}

object ArrayType {
  val name = "Array"
  def apply(et: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(ArrayType.name, et)
  def unapply(x: Any)(implicit tm: TypeMap): Option[TypeExp] = x match {
    case Reflect(ArrayType.name, t :: Nil) => Some(t)
  }
}

object ImplicitScalaTypes {
  import TypeConstants._
  implicit val TUnit:     SimpleType[Unit]    = _ => Unit
  implicit val TBoolean:  SimpleType[Boolean] = _ => Boolean
  implicit val TChar:     SimpleType[Char]    = _ => Char
  implicit val TShort:    SimpleType[Short]   = _ => Short
  implicit val TInt:      SimpleType[Int]     = _ => Int
  implicit val TFloat:    SimpleType[Float]   = _ => Float
  implicit val TDouble:   SimpleType[Double]  = _ => Double

  implicit def mkTArray[T](implicit te: SimpleType[T]): SimpleType[Array[T]] =
    (tm: TypeMap) => ArrayType(te.reify(tm))(tm)
}


case class Tensor[T:Type](shape: Const)

object TensorType {
  val name = "Tensor"

  def apply(et: TypeExp, shape: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(TensorType.name, et, shape)

  def unapply(x: Any)(implicit tm: TypeMap): Option[TypeExp] = x match {
    case Reflect(TensorType.name, t :: Nil) => Some(t)
  }

  implicit def mkTensorType[T:SimpleType]: Type[Tensor[T]] = (_: Exp, tm: TypeMap) =>
    TensorType(implicitly[SimpleType[T]].reify(tm), Const(10) /* shape */)(tm)
}


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

object Main {

  def main(args: Array[String]): Unit = {
    import ImplicitScalaTypes._
    import TypeConstants._
    implicit val tm: TypeMap = TypeMap()

    def Wrap[A:Type](x: lms.core.Backend.Exp): Unit = tm(x) = TypeExp.of[A](x)

    val s1 = Sym(10)
    val s2 = Sym(20)

    Wrap[Array[Array[Float]]](s1)
    Wrap[Array[Array[Int]]](s2)

    tm(s1) match {
      case ArrayType(ArrayType(t)) => println(s"element type $t")
    }

    tm(s2) match {
      case ArrayType(ArrayType(Int)) => println("is Int")
    }

    println(tm)
    println(tm.g.globalDefsCache)
  }

}