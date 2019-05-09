// Raw:
x5 = (!= x4 0)
x7 = (- 2 1)
x8 = (- x4 x7)
x9 = (@ x2 x8)  [CTRL*: _ | x6]
x10 = (* x4 x9)
x12 = (? x5 Block(List(),x10,x6,[CTRL*: _ | x9]) Block(List(),1,x11,[: _ | x11]))  [CTRL*: _ | x3]
x2 = (λ Block(List(x4),x12,x3,[CTRL*: _ | x12]))
x13 = (@ x2 x1)  [CTRL*: _ | x0]
// Generic Codegen:
// in: x1 effect: x0
x7 = (- 2 1)
x2 = (λ {
  // in: x4 effect: x3
  x5 = (!= x4 0)
  x12 = (? x5 {// in:  effect: x6
    x8 = (- x4 x7)
    x9 = (@ x2 x8) // Eff: [CTRL*: _ | x6]
    x10 = (* x4 x9)
    x10 // out effect: [CTRL*: _ | x9]
  } {// in:  effect: x11
    1 // out effect: [: _ | x11]
  }) // Eff: [CTRL*: _ | x3]
  x12 // out effect: [CTRL*: _ | x12]
})
x13 = (@ x2 x1) // Eff: [CTRL*: _ | x0]
x13 // out effect: [CTRL*: _ | x13]
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
{
  val x0 = 2 - 1
  def x1(x2: Int): Int = if (x2 != 0) x2 * x1(x2 - x0) else 1
  x1(x3)
}
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
