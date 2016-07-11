package scala.lms

import scala.annotation.implicitNotFound


trait CodeBuilder {
  // mutable state goes here ...

  var numVars = 0
  var mode = 0 // 0 anf, 1 normal

  case class Exp(s: String) { override def toString = s }
  def reflect(s: Any*) = mode match {
    case 0 => numVars += 1; println("val x"+numVars+" = "+s.mkString); Exp("x"+numVars)
    case 1 => Exp(s.mkString)
  }

  def reifyBlock(f: => Exp) = {
    val source = new java.io.ByteArrayOutputStream()
    val res = scala.Console.withOut(new java.io.PrintStream(source))(f)
    val str = source.toString + res.s
    if (str.contains("\n"))
      Exp("{\n" + str + "\n}")
    else 
      Exp(str)
  }

  def reifyPattern[T](f: => T):T = {
    var save = mode
    mode = 1
    try {
      val source = new java.io.ByteArrayOutputStream()
      val res = scala.Console.withOut(new java.io.PrintStream(source))(f)
      assert(source.toString == "")
      res
    } finally mode = save
  }
  
}



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

trait BaseExp extends Base {

  val codeBuilder: CodeBuilder
  import codeBuilder.Exp

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

  def reflect[T:Typ](s:Any*):T = typ[T].from(codeBuilder.reflect(s:_*))
  def ref[T:Typ](f: => T): Exp = codeBuilder.reifyBlock(typ[T].to(f))

  //case class Rewrite[T:Typ](a:T, b:T)

  def lower[A:Typ,B:Typ,C:Typ](f: (A,B) => Rewrite[C]): Unit = {
    val a = typ[A].from(Exp("?A"))
    val b = typ[B].from(Exp("?B"))
    val rw = codeBuilder.reifyPattern(f(a,b))
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
    def &&(y: => Boolean): Boolean
    def ||(y: => Boolean): Boolean
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

  def __ifThenElse[C,A,B](c:Boolean, a: =>A, b: =>B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Typ[C]): C

}

trait Impl extends BaseExp with DSL {
  val codeBuilder = new CodeBuilder {}
  import codeBuilder.Exp
  case class Int(e: Exp) extends IntOps {
    def +(y: Int) = reflect[Int](e,"+",y.e)
    def -(y: Int) = reflect[Int](e,"-",y.e)
    def *(y: Int) = reflect[Int](e,"*",y.e)
    def /(y: Int) = reflect[Int](e,"/",y.e)
    def %(y: Int) = reflect[Int](e,"%",y.e)
  }
  case class Boolean(e: Exp) extends BooleanOps {
    def &&(y: => Boolean) = reflect[Boolean](e,"&&",y.e)
    def ||(y: => Boolean) = reflect[Boolean](e,"+",y.e)
    def unary_! = reflect[Boolean]("!",e)
  }

  implicit val unitTyp: Typ[Unit] = new Typ[Unit] { def from(e:Exp) = (); def to(x:Unit) = Exp("()"); override def toString = "Unit" }
  implicit val intTyp: Typ[Int] = new Typ[Int] { def from(e:Exp) = Int(e); def to(x:Int) = x.e; override def toString = "Int" }
  implicit val booleanTyp: Typ[Boolean] = new Typ[Boolean] { def from(e:Exp) = Boolean(e); def to(x:Boolean) = x.e; override def toString = "Boolean" }

  implicit val intLift: Lift[scala.Int,Int] = new Lift[scala.Int,Int] { def to(x:scala.Int) = Int(Exp(x.toString)) }
  implicit val booleanLift: Lift[scala.Boolean,Boolean] = new Lift[scala.Boolean,Boolean] { def to(x:scala.Boolean) = Boolean(Exp(x.toString)) }
  
  case class Array[T:Typ](e: Exp) extends ArrayOps[T] {
    def length = reflect[Int](e,".length")
    def apply(x: Int) = reflect[T](e,"(",ref(x),")")
    def update(x: Int, y: T): Unit = reflect[Unit](e,"(",ref(x),") = ",ref(y))
  }
  def NewArray[T:Typ](x: Int): Array[T] = reflect[Array[T]]("new Array[",typ[T],"](",ref(x),")")
  implicit def arrayTyp[T:Typ]: Typ[Array[T]] = new Typ[Array[T]] { def from(e:Exp) = Array(e); def to(x:Array[T]) = x.e; override def toString = "Array["+typ[T]+"]" }

  def __ifThenElse[C,A,B](c:Boolean, a: =>A, b: =>B)(implicit mA: Lift[A,C], mB: Lift[B,C], mC: Typ[C]): C = {
    reflect[C]("if (",ref(c),") ",ref(mA.to(a))," else ",ref(mB.to(b)))
  }
}


