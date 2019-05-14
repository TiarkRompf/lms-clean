class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    def x8(x6: Int): Int = {
      val x7 = x6 * 2
      x7
    }
    val x18 = {
      var x4 = x3
      def x15(x10: Int): Int = {
        val x11 = x4
        val x12 = x8(x11)
        val x13 = x8(x12)
        val x14 = x10 + x13
        x14
      }
      val x16 = x15(x1)
      val x17 = x15(x16)
      x17
    }
    val x19 = x18 + 5
    x19 /*exit: x19 */
  }
}
// output:
