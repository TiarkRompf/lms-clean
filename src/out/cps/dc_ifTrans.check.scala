class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    val x3 = x1 - 5
    val x4 = x1 + 5
    val x24 = {
      var x6 = x2
      def x20(x8: Int): Int = {
        val x9 = x6
        val x10 = x9 > 5
        def x14(x12: Int): Int = {
          val x13 = x8 * x12
          x13
        }
        val x19 = if (x10) {
          val x16 = x14(x3)
          x16
        } else {
          val x18 = x14(x4)
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
