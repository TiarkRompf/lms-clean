// Raw:
x5 = (!= x4 Const(0))
x7 = (- Const(2) Const(1))
x8 = (- x4 x7)
x9 = (@ x2 x8 Eff(List(x6)))
x10 = (* x4 x9)
x12 = (? x5 Block(List(x6),x10,List(x9)) Block(List(x11),Const(1),List(x11)) Eff(List(x3)))
x2 = (λ Block(List(x4, x3),x12,List(x12)))
x13 = (@ x2 x1 Eff(List(x0)))
// Generic Codegen:
// in: List(x1, x0)
x7 = (- Const(2) Const(1))
x2 = (λ {
  // in: List(x4, x3)
  x5 = (!= x4 Const(0))
  x12 = (? x5 {// in: List(x6)
    x8 = (- x4 x7)
    x9 = (@ x2 x8 Eff(List(x6)))
    x10 = (* x4 x9)
    x10 // out effect: List(x9)
  } {// in: List(x11)
    Const(1) // out effect: List(x11)
  } Eff(List(x3)))
  x12 // out effect: List(x12)
})
x13 = (@ x2 x1 Eff(List(x0)))
x13 // out effect: List(x13)
// Scala Codegen:
val x7 = 2 - 1
def x2(x4: Int): Int = {
  val x5 = x4 != 0
  val x12 = if (x5) {
    val x8 = x4 - x7
    val x9 = x2(x8)
    val x10 = x4 * x9
    x10
  } else {
    1
  }
  x12
}
val x13 = x2(x1)
x13
// Compact Scala Codegen:
val x7 = 2 - 1
def x2(x4: Int): Int = if (x4 != 0) x4 * x2(x4 - x7) else 1
x2(x1)
// Generated code
class backend_fac_02 extends (Int => Int) {
  def apply(x0: Int): Int = {
    val x1 = 2 - 1
    def x2(x3: Int): Int = if (x3 != 0) x3 * x2(x3 - x1) else 1
    x2(x0)
  }
}
compilation: ok
// Output:
1
1
2
6
24
