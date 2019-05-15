class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 > 5
    def x6(x4: Int): Int = {
      x4 /*exit: x4 */
    }
    val x22 = if (x2) {
      def x12(x10: Int): Int = {
        val x11 = x10 * 2
        x11
      }
      val x16 = {
        val x13 = x12(x1)
        val x14 = x12(x13)
        val x15 = x14 + 3
        x15
      }
      val x17 = x16 + 10
      val x18 = x6(x17)
      x18
    } else {
      val x20 = x1 + 5
      val x21 = x6(x20)
      x21
    }
    x22
  }
}
// output:
