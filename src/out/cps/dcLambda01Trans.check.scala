class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x2: Int => Int, x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = x2(x5)
      x6
    }
    def x17(x10: Int): Int = {
      val x11 = x10 + 2
      def x15(x13: Int): Int = {
        val x14 = x13 + 3
        x14
      }
      val x16 = x7(x15, x11)
      x16
    }
    val x20 = {
      val x18 = x17(x1)
      val x19 = x17(x18)
      x19
    }
    val x21 = x20 + 5
    x21 /*exit: x21 */
  }
}
// output:
