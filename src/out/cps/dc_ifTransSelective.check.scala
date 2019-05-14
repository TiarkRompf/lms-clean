class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    val x10 = x1 - 5
    val x12 = x1 + 5
    val x19 = {
      var x4 = x3
      def x15(x6: Int): Int = {
        val x7 = x4
        val x8 = x7 > 5
        val x13 = if (x8) {
          x10
        } else {
          x12
        }
        val x14 = x6 * x13
        x14
      }
      val x16 = x15(x1)
      val x17 = x15(x16)
      val x18 = x17 + 3
      x18
    }
    val x20 = x19 + 10
    x20 /*exit: x20 */
  }
}
// output:
