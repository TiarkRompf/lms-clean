// Generated code
class backend_codemotion_03 extends (Int => Int) {
  def apply(x0: Int): Int = {
    def x1(x2: Int): Int = {
      2 * x2}
    def x3(x4: Int): Int = {
      4 * x4}
    val x5 = if (1 != 0) {x1(1)} else {x3(2)}
    def x6(x7: Int): Int = {
      x7 + 1}
    def x8(x9: Int): Int = {
      x9 + 2}
    val x10 = if (0 != 0) {x6(3)} else {x8(4)}
    ({ x11: Int => println(1337)
      if (x11 != 0) {println(1)
        x5} else {println(0)
        x10}})(5)
  }
}
compilation: ok
// Output:
1337
1
2
1337
1
2
1337
1
2
1337
1
2
1337
1
2
