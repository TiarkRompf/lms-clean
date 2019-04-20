// Generated code
class backend_codemotion_02 extends (Int => Int) {
  def apply(x0: Int): Int = {
    val x1 = ((x2: Int) => 2 * x2)(1)
    val x3 = ((x4: Int) => x4 + 1)(2)
    ({ (x5: Int) =>
      println("1337")
      if (x5 != 0) x1 else x3
    })(3)
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
