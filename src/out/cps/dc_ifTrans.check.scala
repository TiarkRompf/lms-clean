class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    val x3 = x2 > 5
    def x17(x5: Int): Int = {
      def x10(x8: Int): Int = {
        val x9 = x8 * x5
        x9
      }
      val x14 = {
        val x11 = x10(x1)
        val x12 = x10(x11)
        val x13 = x12 + 3
        x13
      }
      val x15 = x14 + 10
      x15 /*exit: x15 */
    }
    val x24 = if (x3) {
      val x19 = x1 - 5
      val x20 = x17(x19)
      x20
    } else {
      val x22 = x1 + 5
      val x23 = x17(x22)
      x23
    }
    x24
  }
}
// output:
