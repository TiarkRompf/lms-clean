// Raw:
x6 = (- x4 Const(1))
x7 = (@ x2 x6 x5)
x8 = (* x4 x7)
x10 = (? x4 Block(List(x5),x8,x7) Block(List(x9),Const(1),x9))
x2 = (λ Block(List(x4, x3),x10,x3))
x11 = (@ x2 x1 x0)
// Generic Codegen:
// in: List(x1, x0)
x2 = (λ {
  // in: List(x4, x3)
  x10 = (? x4 {// in: List(x5)
    x6 = (- x4 Const(1))
    x7 = (@ x2 x6 x5)
    x8 = (* x4 x7)
    x8 // out effect: x7
  } {// in: List(x9)
    Const(1) // out effect: x9
  })
  x10 // out effect: x3
})
x11 = (@ x2 x1 x0)
x11 // out effect: x11
// Scala Codegen:
def x2(x4: Int): Int = {
  val x10 = if (x4 != 0) {
    val x6 = x4 - 1
    val x7 = x2(x6)
    val x8 = x4 * x7
    x8
  } else {
    1
  }
  x10
}
val x11 = x2(x1)
x11
// Compact Scala Codegen:
def x2(x4: Int): Int = {
  if (x4 != 0) {x4 * x2(x4 - 1)} else {1}}
x2(x1)
// Generated code
class backend_fac_01 extends (Int => Int) {
  def apply(x0: Int): Int = {
    def x1(x2: Int): Int = {
      if (x2 != 0) {x2 * x1(x2 - 1)} else {1}}
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
