class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 > 5
    val x16 = if (x2) {
      def x8(x6: Int): Int = {
        val x7 = x6 * 2
        x7
      }
      val x9 = x8(x1)
      val x10 = x8(x9)
      val x11 = x10 + 3
      val x12 = {
        x11
      }
      val x13 = x12 + 10
      x13
    } else {
      val x15 = x1 + 5
      x15
    }
    x16 /*exit: x16 */
  }
}
// output:
