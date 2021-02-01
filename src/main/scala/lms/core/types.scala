package lms.core

import lms.core.Backend.{Const, Exp, Node, Sym}
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
  val Unit, Boolean, Char, Short, Int, Float, Double = Value
}

// Primitive types corresponding to Scala ones
object Types {

  val Unit: TypeExp = Const(TypeNames.Unit)
  val Boolean: TypeExp = Const(TypeNames.Boolean)
  val Char: TypeExp = Const(TypeNames.Char)
  val Short: TypeExp = Const(TypeNames.Short)
  val Int: TypeExp = Const(TypeNames.Int)
  val Float: TypeExp = Const(TypeNames.Float)
  val Double: TypeExp = Const(TypeNames.Double)


  object Array {
    val name = "Array"

    def apply(et: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(Array.name, et)

    def unapply(x: Any)(implicit tm: TypeMap): Option[TypeExp] = x match {
      case Reflect(Array.name, t :: Nil) => Some(t)
    }
  }

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

object ImplicitScalaTypes {
  import Types._
  implicit val mkUnitType:     SimpleType[Unit]    = _ => Unit
  implicit val mkBooleanType:  SimpleType[Boolean] = _ => Boolean
  implicit val mkCharType:     SimpleType[Char]    = _ => Char
  implicit val mkShortType:    SimpleType[Short]   = _ => Short
  implicit val mkIntType:      SimpleType[Int]     = _ => Int
  implicit val mkFloatType:    SimpleType[Float]   = _ => Float
  implicit val mkDoubleType:   SimpleType[Double]  = _ => Double

  implicit def mkArrayType[T](implicit te: SimpleType[T]): SimpleType[Array[T]] =
    (tm: TypeMap) => Array(te.reify(tm))(tm)
}


// Types and IR nodes defined outside of LMS core
case class Tensor[T:Type](shape: Const)

object TensorType {
  val name = "Tensor"

  def apply(et: TypeExp, shape: TypeExp)(implicit tm: TypeMap): TypeExp = tm.g.reflect(TensorType.name, et, shape)

  def unapply(x: Any)(implicit tm: TypeMap): Option[(TypeExp, TypeExp)] = x match {
    case Reflect(TensorType.name, t :: shape :: Nil) => Some((t, shape))
  }

  implicit def mkTensorType[T:SimpleType]: Type[Tensor[T]] = (t: Exp, tm: TypeMap) => {
    val deff = tm.g.findDefinition(t)
    val shape = deff match {
      case Some(Node(_, "Tensor", rhs, eff)) => Const(rhs.head)
      case None => throw new Exception()
    }
    TensorType(implicitly[SimpleType[T]].reify(tm), shape)(tm)
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    import ImplicitScalaTypes._
    import TensorType.mkTensorType
    val ty = Types
    implicit val tm: TypeMap = TypeMap()

    def Wrap[A:Type](x: lms.core.Backend.Exp): Unit = tm(x) = TypeExp.of[A](x)

    val s1 = Sym(1)
    val s2 = Sym(2)
    val s3 = tm.g.reflect("Tensor", Const(/* shape = */List(130, 20)))

    Wrap[Array[Array[Float]]](s1)
    Wrap[Array[Array[Int]]](s2)
    Wrap[Tensor[Float]](s3)

    tm(s1) match {
      case ty.Array(ty.Array(t)) => println(s"element type $t")
    }

    tm(s2) match {
      case ty.Array(ty.Array(ty.Int)) => println("is Int")
    }

    tm(s3) match {
      case TensorType(et, shape) => println(s"Tensor type $et $shape")
    }

    println(tm)
    println(tm.g.globalDefsCache)
  }

}
