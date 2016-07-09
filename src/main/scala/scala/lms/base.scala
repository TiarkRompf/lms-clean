package scala.lms

import scala.annotation.implicitNotFound

trait Base {
  // preliminaries
  @implicitNotFound("${T} is not a DSL type")
  type Typ[T]
  @implicitNotFound("${A} cannot be implicitly lifted to ${B}")
  type Lift[A,B]
  implicit def identLift[T:Typ]: Lift[T,T]
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U

  case class Rewrite[T:Typ](a:T, b:T)

  def lower[A:Typ,B:Typ,C:Typ](f: (A,B) => Rewrite[C]): Unit
}

trait Expressions {

  var numVars = 0
  case class Exp(s: String) { override def toString = s }
  implicit def reflectS(s: String) = { numVars += 1; println("val x"+numVars+" = "+s); Exp("x"+numVars) }

}

trait BaseExp extends Base with Expressions {

  trait Typ[T] {
    def from(e:Exp): T
    def to(x:T):Exp
  }
  trait Lift[A,B] {
    def to(x:A):B
  }
  implicit def identLift[T:Typ]: Lift[T,T] = new Lift[T,T] { def to(x:T) = x }
  implicit def lift[T,U](x:T)(implicit e: Lift[T,U]): U = e.to(x)

  def typ[T:Typ] = implicitly[Typ[T]]


  // macro / auto-lift support
  case class Thunk(f: () => Exp) {
    override def toString = "<"+f()+">"
  }

  def reflect[T:Typ](s:String,xs: Thunk*):T = typ[T].from(Exp(s+","+xs.mkString(",")))
  def ref[T:Typ](f: => T): Thunk = Thunk(() => typ[T].to(f))

  //case class Rewrite[T:Typ](a:T, b:T)

  def lower[A:Typ,B:Typ,C:Typ](f: (A,B) => Rewrite[C]): Unit = {
    val a = typ[A].from(Exp("?A"))
    val b = typ[B].from(Exp("?B"))
    val rw = f(a,b)
    val u = typ[C].to(rw.a)
    val v = typ[C].to(rw.b)
    println("lower: " + u + "===>" + v)
  }
}


trait DSL extends Base {
  trait IntOps {
    def +(y: Int): Int
    def -(y: Int): Int
    def *(y: Int): Int
    def /(y: Int): Int
    def %(y: Int): Int
  }
  trait BooleanOps {
    def &&(y: Boolean): Boolean
    def ||(y: Boolean): Boolean
    def unary_! : Boolean
  }
  type Int <: IntOps
  type Boolean <: BooleanOps
  implicit def intTyp: Typ[Int]
  implicit def intLift: Lift[scala.Int,Int]
  implicit def booleanTyp: Typ[Boolean]
  implicit def booleanLift: Lift[scala.Boolean,Boolean]

  trait ArrayOps[T] {
    def length: Int
    def apply(x: Int): T
    def update(x: Int, y: T): Unit
  }
  type Array[T] <: ArrayOps[T]
  def NewArray[T:Typ](x: Int): Array[T]
  implicit def arrayTyp[T:Typ]: Typ[Array[T]]

  def __ifThenElse[C,A,B](c:Boolean, a:A, b:B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Typ[C]): C

}

trait Impl extends BaseExp with DSL {
  case class Int(e: Exp) extends IntOps {
    def +(y: Int) = Int(e+"+"+y.e)
    def -(y: Int) = Int(e+"-"+y.e)
    def *(y: Int) = Int(e+"*"+y.e)
    def /(y: Int) = Int(e+"/"+y.e)
    def %(y: Int) = Int(e+"%"+y.e)
  }
  case class Boolean(e: Exp) extends BooleanOps {
    def &&(y: Boolean) = Boolean(e+"+"+y.e)
    def ||(y: Boolean) = Boolean(e+"+"+y.e)
    def unary_! = Boolean("!"+e)
  }

  val intTyp: Typ[Int] = new Typ[Int] { def from(e:Exp) = Int(e); def to(x:Int) = x.e; override def toString = "Int" }
  val booleanTyp: Typ[Boolean] = new Typ[Boolean] { def from(e:Exp) = Boolean(e); def to(x:Boolean) = x.e; override def toString = "Boolean" }

  val intLift: Lift[scala.Int,Int] = new Lift[scala.Int,Int] { def to(x:scala.Int) = Int(Exp(x.toString)) }
  val booleanLift: Lift[scala.Boolean,Boolean] = new Lift[scala.Boolean,Boolean] { def to(x:scala.Boolean) = Boolean(Exp(x.toString)) }
  
  case class Array[T:Typ](e: Exp) extends ArrayOps[T] {
    def length = Int(e+".length")
    def apply(x: Int) = typ[T].from(e+"("+x.e+")")
    def update(x: Int, y: T): Unit = reflectS(e+"("+x.e+") = "+typ[T].to(y))
  }
  def NewArray[T:Typ](x: Int): Array[T] = Array("new Array["+typ[T]+"]("+x.e+")")
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = new Typ[Array[T]] { def from(e:Exp) = Array(e); def to(x:Array[T]) = x.e; override def toString = "Array["+typ[T]+"]" }

  def __ifThenElse[C,A,B](c:Boolean, a:A, b:B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Typ[C]): C = {
    mC.from("if ("+c.e+") "+mC.to(mA.to(a))+" else "+mC.to(mB.to(b)))
  }
}


