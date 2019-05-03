class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 + x1
    val x3 = x2 * x1
    val x4 = x3 / x3
    x4 /*exit x4*/
  }
}
// output:
