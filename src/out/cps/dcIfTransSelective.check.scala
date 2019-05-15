class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = 1 + x5
      x6
    }
    val x8 = x1 > 5
    val x17 = {
      val x15 = if (x8) {
        val x10 = x7(x1)
        val x11 = x7(x10)
        x11
      } else {
        val x13 = x7(x1)
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
