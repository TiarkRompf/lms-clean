class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x8 = x1 > 5
    val x14 = if (x8) {
      def x7(x4: Int): Int = {
        val x5 = x4 * 2
        val x6 = 1 + x5
        x6
      }
      val x10 = x7(x1)
      val x11 = x7(x10)
      x11
    } else {
      def x7(x4: Int): Int = {
        val x5 = x4 * 2
        val x6 = 1 + x5
        x6
      }
      val x10 = x7(x1)
      val x13 = x10 + 5
      x13
    }
    val x15 = 3 + x14
    val x16 = {
      x15
    }
    val x17 = x16 + 4
    val x18 = x17 + 10
    x18 /*exit: x18 */
  }
}
// output:
