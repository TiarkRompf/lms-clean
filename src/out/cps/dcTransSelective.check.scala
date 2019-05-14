class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x12(x4: Int): Int = {
      def x9(x6: Int): Int = {
        val x7 = x4 + x6
        val x8 = x7 + 4
        x8
      }
      val x10 = x9(x1)
      val x11 = 3 * x10
      x11
    }
    val x13 = x12(x1)
    val x14 = 2 * x13
    val x15 = {
      x14
    }
    val x16 = x15 * x1
    x16 /*exit: x16 */
  }
}
// output:
