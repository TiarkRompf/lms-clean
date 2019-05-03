class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    def x6(x4: Int): Int = {
      val x5 = x4 * 2
      x5
    }
    val x18 = {
      var x8 = x2
      def x15(x10: Int): Int = {
        val x11 = x8
        val x12 = x6(x11)
        val x13 = x6(x12)
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
