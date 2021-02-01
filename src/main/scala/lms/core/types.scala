package lms.core

import lms.core.Backend.{Const, Exp, Sym}
import lms.core.TypeMap.TypeExp

import scala.collection.mutable

object BaseTypeNames {
  val Unit        = "Unit"
  val Boolean     = "Boolean"
  val Char        = "Char"
  val Short       = "Short"
  val Int         = "Int"
  val Float       = "Float"
  val Double      = "Double"
  val String      = "String"
  val Array       = "Array"
}

object Reflect {
  def unapply(x: Any)(implicit tm: TypeMap): Option[(String, List[Exp])] = x match {
    case s@Sym(_) =>
      tm.g.findDefinition(s).map(n => (n.op, n.rhs.filter(_.isInstanceOf[Exp]).map(_.asInstanceOf[Exp])))
    case _ => None
  }
}

abstract class Type[T] {
  def reify(x: Exp, tm: TypeMap): TypeMap.TypeExp
}

abstract class SimpleType[T] extends Type[T] {
  def reify(x: Exp, tm: TypeMap): TypeMap.TypeExp = reify(tm)
  def reify(tm: TypeMap): TypeMap.TypeExp
}

object TypeExp {
  def of[T](x: Exp)(implicit t: Type[T], typeMap: TypeMap): TypeMap.TypeExp = t.reify(x, typeMap)
}

object TypeNames {
  // TODO maybe Int?
  val Unit:     String = "Unit"
  val Boolean:  String = "Boolean"
  val Char:     String = "Char"
  val Short:    String = "Short"
  val Int:      String = "Int"
  val Float:    String = "Float"
  val Double:   String = "Double"

  val Array:    String = "Array"
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

object ImplicitTypes {
  import TypeConstants._
  implicit val TUnit:     SimpleType[Unit]    = _ => Unit
  implicit val TBoolean:  SimpleType[Boolean] = _ => Boolean
  implicit val TChar:     SimpleType[Char]    = _ => Char
  implicit val TShort:    SimpleType[Short]   = _ => Short
  implicit val TInt:      SimpleType[Int]     = _ => Int
  implicit val TFloat:    SimpleType[Float]   = _ => Float
  implicit val TDouble:   SimpleType[Double]  = _ => Double

  object Array {
    def unapply(x: Any)(implicit tm: TypeMap): Option[TypeExp] = x match {
      case Reflect(TypeNames.Array, t :: Nil) => Some(t)
    }
  }

  implicit def mkTArray[T](implicit te: SimpleType[T]): SimpleType[Array[T]] =
    (tm: TypeMap) => tm.g.reflect(TypeNames.Array, te.reify(tm))
}

case class Tensor[T:Type](shape: Const)

object Tensor {
  val name = "Tensor"

  def unapply(x: Any)(implicit tm: TypeMap): Option[TypeExp] = x match {
    case Reflect(Tensor.name, t :: Nil) => Some(t)
  }

  implicit def mkTensorType[T:SimpleType]: Type[Tensor[T]] = (_: Exp, tm: TypeMap) =>
    tm.g.reflect(Tensor.name, implicitly[SimpleType[T]].reify(tm)/*, shape */)
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
    import TypeConstants._
    import ImplicitTypes._
    implicit val tm: TypeMap = TypeMap()

    def Wrap[A:Type](x: lms.core.Backend.Exp): Unit = tm(x) = TypeExp.of[A](x)

    val s1 = Sym(10)
    val s2 = Sym(20)

    Wrap[Array[Array[Float]]](s1)
    Wrap[Array[Array[Int]]](s2)

    tm(s1) match {
      case Array(Array(t)) => println(s"element type $t")
    }

    tm(s2) match {
      case Array(Array(Int)) => println("is Int")
    }

    println(tm)
    println(tm.g.globalDefsCache)
  }

}