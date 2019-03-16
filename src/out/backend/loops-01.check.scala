// Generated code
class backend_loops_01 extends (Int => Int) {
  def apply(x0: Int): Int = {
    val x1 = new Array[Int](x0)
    var x2 = 0
    var x3 = 0
    while (x2 != x0) {
      x3 = x3 + x2
      x1(x2) = x3
      x2 = x2 + 1
    }
    x3
  }
}
compilation: ok
// Output:
0
0
1
3
6
