// Raw:
x5 = (!= x4 Const(0))
x7 = (- x4 Const(1))
x8 = (@ x2 x7 Eff(x6))
x9 = (* x4 x8)
x11 = (? x5 Block(List(x6),x9,x8) Block(List(x10),Const(1),x10))
x2 = (λ Block(List(x4, x3),x11,x3))
x12 = (@ x2 x1 Eff(x0))
// Generic Codegen:
// in: List(x1, x0)
x2 = (λ {
  // in: List(x4, x3)
  x5 = (!= x4 Const(0))
  x11 = (? x5 {// in: List(x6)
    x7 = (- x4 Const(1))
    x8 = (@ x2 x7 Eff(x6))
    x9 = (* x4 x8)
    x9 // out effect: x8
  } {// in: List(x10)
    Const(1) // out effect: x10
  })
  x11 // out effect: x3
})
x12 = (@ x2 x1 Eff(x0))
x12 // out effect: x12
// Scala Codegen:
def x2(x4: Int): Int = {
  val x5 = x4 != 0
  val x11 = if (x5) {
    val x7 = x4 - 1
    val x8 = x2(x7)
    val x9 = x4 * x8
    x9
  } else {
    1
  }
  x11
}
val x12 = x2(x1)
x12
// Compact Scala Codegen:
def x2(x4: Int): Int = if (x4 != 0) x4 * x2(x4 - 1) else 1
x2(x1)
// Generated code
class backend_fac_01 extends (Int => Int) {
  def apply(x0: Int): Int = {
    def x1(x2: Int): Int = if (x2 != 0) x2 * x1(x2 - 1) else 1
    x1(x0)
  }
}
compilation: ok
// Output:
1
1
2
6
24
