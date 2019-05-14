class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 > 5
    val x13 = x1 - 5
    val x24 = x1 + 5
    val x29 = {
      def x6(x5: Int): Int = {
        x5
      }
      val x28 = if (x3) {
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
        def x23(x19: Int): Int = {
          val x20 = x19 * 2
          val x21 = x20 + 2
          val x22 = x6(x21)
          x22
        }
        val x25 = x23(x24)
        val x26 = x23(x25)
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
