class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    val x14 = x1 - 5
    val x17 = x1 + 5
    val x24 = {
      var x4 = x3
      def x20(x6: Int): Int = {
        val x7 = x4
        val x8 = x7 > 5
        def x12(x10: Int): Int = {
          val x11 = x6 * x10
          x11
        }
        val x19 = if (x8) {
          val x15 = x12(x14)
          x15
        } else {
          val x18 = x12(x17)
          x18
        }
        x19
      }
      val x21 = x20(x1)
      val x22 = x20(x21)
      val x23 = x22 + 3
      x23
    }
    val x25 = x24 + 10
    x25 /*exit: x25 */
  }
}
// output:
