class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = 1 + x5
      x6
    }
    val x8 = x1 > 5
    val x14 = x7(x1)
    val x15 = x7(x14)
    val x18 = x14 + 5
    val x21 = {
      def x12(x10: Int): Int = {
        val x11 = 3 + x10
        x11
      }
      val x20 = if (x8) {
        val x16 = x12(x15)
        x16
      } else {
        val x19 = x12(x18)
        x19
      }
      x20
    }
    val x22 = x21 + 4
    val x23 = x22 + 10
    x23 /*exit: x23 */
  }
}
// output:
