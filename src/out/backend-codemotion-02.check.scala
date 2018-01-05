// Generated code
class backend_codemotion_02 extends (Int => Int) {
  def apply(x0: Int): Int = {
    def x1(x2: Int): Int = {
      2 * x2}
    def x3(x4: Int): Int = {
      x4 + 1}
    ({ x5: Int => println(1337)
      if (x5 != 0) {x1(1)} else {x3(2)}})(3)
  }
}
compilation: ok
// Output:
1337
2
1337
2
1337
2
1337
2
1337
2
