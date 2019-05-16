class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 > 5
    def x8(x5: Int): Int = {
      val x6 = x5 * 2
      val x7 = 1 + x6
      x7
    }
    val x17 = {
      val x15 = if (x2) {
        val x10 = x8(x1)
        val x11 = x8(x10)
        x11
      } else {
        val x13 = x8(x1)
        val x14 = x13 + 5
        x14
      }
      val x16 = 3 + x15
      x16
    }
    val x18 = x17 + 4
    val x19 = x18 + 10
    x19 /*exit: x19 */
  }
}
// output:
