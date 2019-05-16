class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 > 5
    val x3 = x1 - 5
    val x4 = x1 + 100
    val x5 = x4 + 2
    val x22 = {
      def x9(x8: Int): Int = {
        x8
      }
      val x21 = if (x2) {
        def x15(x12: Int): Int = {
          val x13 = x12 * 3
          val x14 = x9(x13)
          x14
        }
        val x16 = x15(x3)
        val x17 = x15(x16)
        val x18 = x17 + 7
        x18
      } else {
        val x20 = x9(x5)
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
