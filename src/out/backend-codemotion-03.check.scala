// Generated code
class backend_codemotion_03 extends (Int => Int) {
  def apply(x0: Int): Int = {
    val x1 = if (true) ((x2: Int) => 2 * x2)(1) else ((x3: Int) => 4 * x3)(2)
    val x4 = if (false) ((x5: Int) => x5 + 1)(3) else ((x6: Int) => x6 + 2)(4)
    ({ x7: Int =>
      println("1337")
      if (x7 != 0) {
        println(1)
        x1
      } else {
        println(0)
        x4
      }
    })(5)
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
