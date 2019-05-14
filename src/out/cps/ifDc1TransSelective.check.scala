class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 > 5
    val x13 = x1 - 5
    val x18 = x1 + 100
    val x19 = x18 + 2
    val x22 = {
      def x6(x5: Int): Int = {
        x5
      }
      val x21 = if (x3) {
        def x12(x9: Int): Int = {
          val x10 = x9 * 3
          val x11 = x6(x10)
          x11
        }
        val x14 = x12(x13)
        val x15 = x12(x14)
        val x16 = x15 + 7
        x16
      } else {
        val x20 = x6(x19)
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
