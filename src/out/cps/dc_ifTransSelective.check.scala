class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    val x3 = x1 - 5
    val x4 = x1 + 5
    val x19 = {
      var x6 = x2
      def x15(x8: Int): Int = {
        val x9 = x6
        val x10 = x9 > 5
        val x13 = if (x10) {
          x3
        } else {
          x4
        }
        val x14 = x8 * x13
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
