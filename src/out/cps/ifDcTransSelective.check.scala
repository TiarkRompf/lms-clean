class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 > 5
    val x3 = x1 - 5
    val x4 = x1 + 5
    val x29 = {
      def x8(x7: Int): Int = {
        x7
      }
      val x28 = if (x2) {
        def x14(x11: Int): Int = {
          val x12 = x11 * 3
          val x13 = x8(x12)
          x13
        }
        val x15 = x14(x3)
        val x16 = x14(x15)
        val x17 = x16 + 7
        x17
      } else {
        def x24(x20: Int): Int = {
          val x21 = x20 * 2
          val x22 = x21 + 2
          val x23 = x8(x22)
          x23
        }
        val x25 = x24(x4)
        val x26 = x24(x25)
        val x27 = x26 + 7
        x27
      }
      x28
    }
    val x30 = x29 + 4
    val x31 = x30 + 10
    x31 /*exit: x31 */
  }
}
// output:
