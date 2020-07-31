package lms.experimental

import scala.annotation.implicitNotFound

trait CBase {

  case class Thunk[T](eval: () => T)
  implicit def thunk(x: => Exp) = Thunk(() => x)

  case class Exp(s: String)

  def quote(e: Exp) = e.s

  var numVars = 0
  def fresh = { numVars += 1; Exp("x" + (numVars - 1)) }

  implicit class StringHelper(sc: StringContext) {
    def gen(args: Thunk[Exp]*): Exp = {
      sc.checkLengths(args)
      val start :: contextStrings = sc.parts.toList
      print(start)
      for ((arg, contextString) <- args zip contextStrings) {
        print(quote(arg.eval()))
        print(contextString)
      }
      println()
      Exp("")
    }

    /*
    def cg(args: Thunk[Rep[Any]]*) = new {
      def as[T:Manifest]: Rep[T] = {
        //reflect(c.s(args map (a => reify(a.eval())):_*))
        def merge(a: List[Any], b: List[Any]): List[Any] = a match {
          case Nil => b
          case x::xs => x::merge(b,xs)
        }
        unchecked[T](merge(c.parts.toList, args.toList.map(_.eval())):_*)
      }
    }
     */
  }
}

object c extends CBase {

  @implicitNotFound("${T} is not a DSL type")
  trait Typ[T] {
    def from(e: Exp): T
    def to(x: T): Exp
  }
  @implicitNotFound("${A} cannot be implicitly lifted to ${B}")
  trait Lift[A, B] {
    def to(x: A): B
  }

  implicit def identLift[T: Typ]: Lift[T, T] = new Lift[T, T] { def to(x: T) = x }
  implicit def lift[T, U](x: T)(implicit e: Lift[T, U]): U = e.to(x)

  def typ[T: Typ] = implicitly[Typ[T]]

  def cbreflect[T: Typ](s: Exp): T = {
    val tpe = typ[T]
    val x = fresh
    gen"""${Exp(tpe.toString)} $x = $s}"""
    tpe from x
  }

  def reflect[T: Typ](s: Any*): T = cbreflect[T](Exp(s.mkString))
  def ref[T: Typ](f: => T): Exp = (typ[T].to(f))

  case class Int(e: Exp) {
    def +(y: Int) = reflect[Int](e, "+", y.e)
    def -(y: Int) = reflect[Int](e, "-", y.e)
    def *(y: Int) = reflect[Int](e, "*", y.e)
    def /(y: Int) = reflect[Int](e, "/", y.e)
    def %(y: Int) = reflect[Int](e, "%", y.e)
  }
  case class Boolean(e: Exp) {
    def &&(y: => Boolean) = reflect[Boolean](e, "&&", y.e)
    def ||(y: => Boolean) = reflect[Boolean](e, "+", y.e)
    def unary_! = reflect[Boolean]("!", e)
  }

  implicit val unitTyp: Typ[Unit] = new Typ[Unit] {
    def from(e: Exp) = ();
    def to(x: Unit) = Exp("()");
    override def toString = "Unit"
  }
  implicit val intTyp: Typ[Int] = new Typ[Int] {
    def from(e: Exp) = Int(e);
    def to(x: Int) = x.e;
    override def toString = "Int"
  }
  implicit val booleanTyp: Typ[Boolean] = new Typ[Boolean] {
    def from(e: Exp) = Boolean(e);
    def to(x: Boolean) = x.e;
    override def toString = "Boolean"
  }

  implicit val intLift: Lift[scala.Int, Int] = new Lift[scala.Int, Int] {
    def to(x: scala.Int) = Int(Exp(x.toString))
  }
  implicit val booleanLift: Lift[scala.Boolean, Boolean] = new Lift[scala.Boolean, Boolean] {
    def to(x: scala.Boolean) = Boolean(Exp(x.toString))
  }

  case class Array[T: Typ](e: Exp) {
    def length = reflect[Int](e, ".length")
    def apply(x: Int) = reflect[T](e, "(", ref(x), ")")
    def update(x: Int, y: T): Unit = reflect[Unit](e, "(", ref(x), ") = ", ref(y))
  }
  def NewArray[T: Typ](x: Int): Array[T] = reflect[Array[T]]("new Array[", typ[T], "](", ref(x), ")")
  implicit def arrayTyp[T: Typ]: Typ[Array[T]] =
    new Typ[Array[T]] {
      def from(e: Exp) = Array(e);
      def to(x: Array[T]) = x.e;
      override def toString = "Array[" + typ[T] + "]"
    }

  def __ifThenElse[C, A, B](c: Boolean, a: => A, b: => B)(implicit mA: Lift[A, C], mB: Lift[B, C], mC: Typ[C]): C = {
    reflect[C]("if (", ref(c), ") ", ref(mA.to(a)), " else ", ref(mB.to(b)))
  }

}
