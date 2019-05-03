class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 + x1
    val x3 = x2 > 2
    def x7(x5: Int): Int = {
      x5 /*exit: x5 */
    }
    val x16 = if (x3) {
      val x9 = x2 * x1
      val x10 = x9 + x1
      val x11 = x7(x10)
      x11
    } else {
      val x13 = x2 / x1
      val x14 = x13 - x1
      val x15 = x7(x14)
      x15
    }
    x16
  }
}
// output:
