// Raw:
x6 = (- Const(2) Const(1))
x7 = (- x4 x6)
x8 = (@ x2 x7 x5)
x9 = (* x4 x8)
x11 = (? x4 Block(List(x5),x9) Block(List(x10),Const(1)))
x2 = (λ Block(List(x4, x3),x11))
x12 = (@ x2 x1 x0)
// Generic Codegen:
// in: List(x1, x0)
x6 = (- Const(2) Const(1))
x2 = (λ {
  // in: List(x4, x3)
  x11 = (? x4 {// in: List(x5)
    x7 = (- x4 x6)
    x8 = (@ x2 x7 x5)
    x9 = (* x4 x8)
    x9
  } {// in: List(x10)
    Const(1)
  })
  x11
})
x12 = (@ x2 x1 x0)
x12
// Scala Codegen:
val x6 = 2 - 1
def x2(x4: Int): Int = {
  val x11 = if (x4 != 0) {
    val x7 = x4 - x6
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
val x0 = 2 - 1
def x1(x2: Int): Int = {
  if (x2 != 0) {x2 * x1(x2 - x0)} else {1}}
x1(x3)
// Generated code
class backend_fac_02 extends (Int => Int) {
  def apply(x0: Int): Int = {
    val x1 = 2 - 1
    def x2(x3: Int): Int = {
      if (x3 != 0) {x3 * x2(x3 - x1)} else {1}}
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
