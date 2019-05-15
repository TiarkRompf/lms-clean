class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = 1 + x5
      x6
    }
    val x8 = x1 > 5
    val x22 = {
      def x12(x10: Int): Int = {
        val x11 = 3 + x10
        x11
      }
      val x21 = if (x8) {
        val x14 = x7(x1)
        val x15 = x7(x14)
        val x16 = x12(x15)
        x16
      } else {
        val x18 = x7(x1)
        val x19 = x18 + 5
        val x20 = x12(x19)
        x20
      }
      x21
    }
    val x23 = x22 + 4
    val x24 = x23 + 10
    x24 /*exit: x24 */
  }
}
// output:
