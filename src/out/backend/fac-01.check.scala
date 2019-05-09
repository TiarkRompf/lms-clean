// Raw:
x5 = (!= x4 0)
x7 = (- x4 1)
x8 = (@ x2 x7)  [CTRL*: _ | x6]
x9 = (* x4 x8)
x11 = (? x5 Block(List(),x9,x6,[CTRL*: _ | x8]) Block(List(),1,x10,[: _ | x10]))  [CTRL*: _ | x3]
x2 = (λ Block(List(x4),x11,x3,[CTRL*: _ | x11]))
x12 = (@ x2 x1)  [CTRL*: _ | x0]
// Generic Codegen:
// in: x1 effect: x0
x2 = (λ {
  // in: x4 effect: x3
  x5 = (!= x4 0)
  x11 = (? x5 {// in:  effect: x6
    x7 = (- x4 1)
    x8 = (@ x2 x7) // Eff: [CTRL*: _ | x6]
    x9 = (* x4 x8)
    x9 // out effect: [CTRL*: _ | x8]
  } {// in:  effect: x10
    1 // out effect: [: _ | x10]
  }) // Eff: [CTRL*: _ | x3]
  x11 // out effect: [CTRL*: _ | x11]
})
x12 = (@ x2 x1) // Eff: [CTRL*: _ | x0]
x12 // out effect: [CTRL*: _ | x12]
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
{
  def x0(x1: Int): Int = if (x1 != 0) x1 * x0(x1 - 1) else 1
  x0(x2)
}
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
