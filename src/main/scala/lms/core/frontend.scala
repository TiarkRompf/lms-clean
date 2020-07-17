package lms.core

import Backend._

class FrontEnd {

  var g: GraphBuilder = null

  val CTRL = Const("CTRL")
  val STORE = Const("STORE")
  val CPS = Const("CPS")

  case class BOOL(x: Exp) {
    //def &&(y: => BOOL): BOOL = BOOL(g.reflect("&",x,y.x)) // should call if?
    //def ||(y: => BOOL): BOOL = BOOL(g.reflect("|",x,y.x))
    def unary_! = BOOL(g.reflect("!",x))
  }

  case class UNIT(x: Exp)

  case class INT(x: Exp) {
    def +(y: INT): INT = INT(g.reflect("+",x,y.x))
    def -(y: INT): INT = INT(g.reflect("-",x,y.x))
    def *(y: INT): INT = INT(g.reflect("*",x,y.x))
    def /(y: INT): INT = INT(g.reflect("/",x,y.x))
    def %(y: INT): INT = INT(g.reflect("%",x,y.x))
    def ===(y: INT): BOOL = BOOL(g.reflect("==",x,y.x))
    def !==(y: INT): BOOL = BOOL(g.reflect("!=",x,y.x))
    def sin(): INT = INT(g.reflect("sin", x))
    def cos(): INT = INT(g.reflect("cos", x))
    def abs(): INT = INT(g.reflect("abs", x))
    def tanh(): INT = INT(g.reflect("tanh", x))
    def exp(): INT = INT(g.reflect("exp", x))
    def log(): INT = INT(g.reflect("log", x))
    def sqrt(): INT = INT(g.reflect("sqrt", x))
  }

  case class STRING(x: Exp) {
  }

  case class ARRAY(x: Exp) {
    def apply(i: INT): INT = INT(g.reflectRead("array_get",x,i.x)(x))
    def update(i: INT, y: INT): Unit = g.reflectWrite("array_set",x,i.x,y.x)(x)
  }
  object ARRAY {
    def apply(n: INT): ARRAY = ARRAY(g.reflectMutable("array_new",n.x))
  }

  case class VAR(x: Exp) {
    def apply(): INT = INT(g.reflectRead("var_get",x)(x))
    def update(y: INT): Unit = g.reflectWrite("var_set",x,y.x)(x)
  }
  object VAR {
    def apply(x: INT): VAR = VAR(g.reflectMutable("var_new",x.x))
  }

  def IF(c: BOOL)(a: => INT)(b: => INT): INT = {
    val aBlock = g.reifyHere(a.x)
    val bBlock = g.reifyHere(b.x)
    // compute effect (aBlock || bBlock)
    val pure = aBlock.isPure && bBlock.isPure
    if (pure)
      INT(g.reflect("?",c.x,aBlock,bBlock))
    else
      INT(g.reflectEffectSummaryHere("?",c.x,aBlock,bBlock)(g.mergeEffKeys(aBlock, bBlock)))
  }

  def WHILE(c: => BOOL)(b: => Unit): Unit = {
    val cBlock = g.reify(c.x)
    val bBlock = g.reify({b;Const(())})
    // compute effect (cBlock bBlock)* cBlock
    g.reflectEffectSummary("W",cBlock,bBlock)(g.mergeEffKeys(cBlock, bBlock))
  }


  def APP(f: Exp, x: INT): INT = INT(g.reflect("@", f, x.x))

  def PRINT(x: INT): Unit =
    g.reflectWrite("P",x.x)(CTRL)

  def PRINT(x: STRING): Unit =
    g.reflectWrite("P",x.x)(CTRL)

  def FUN(f: INT => INT): INT => INT = FUN((_,x) => f(x))

  def FUN(f: ((INT=>INT),INT) => INT): INT => INT = {
    val fn = Sym(g.fresh)
    //val xn = Sym(g.fresh)
    val f1 = (x: INT) => APP(fn,x)
    // NOTE: lambda expression itself does not have
    // an effect, so body block should not count as
    // latent effect of the lambda
    g.reflect(fn,"λ",g.reify(xn => f(f1,INT(xn)).x))()
    f1
  }

  implicit def liftInt(x: Int): INT = INT(Const(x))
  implicit def liftBool(x: Boolean): BOOL = BOOL(Const(x))
  implicit def liftString(x: String): STRING = STRING(Const(x))

  def mkGraphBuilder() = new GraphBuilder

  def program(f: INT => INT): Graph = program(g.reify { x => f(INT(x)).x })
  // def program(f: (Exp, Exp) => Exp): Graph = program(g.reify { f })
  def program(body: => Block): Graph = {
    assert(g == null)
    g = mkGraphBuilder()
    try {
      val block = body
      Graph(g.globalDefs, block, g.globalDefsCache.toMap)
    } finally {g = null}
  }


}
