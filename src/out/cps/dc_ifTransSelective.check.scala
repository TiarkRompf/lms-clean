class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    val x3 = x2 > 5
    val x8 = if (x3) {
      val x5 = x1 - 5
      x5
    } else {
      val x7 = x1 + 5
      x7
    }
    def x13(x11: Int): Int = {
      val x12 = x11 * x8
      x12
    }
    val x17 = {
      val x14 = x13(x1)
      val x15 = x13(x14)
      val x16 = x15 + 3
      x16
    }
    val x18 = x17 + 10
    x18 /*exit: x18 */
  }
}
// output:
