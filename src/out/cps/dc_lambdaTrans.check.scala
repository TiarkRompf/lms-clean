class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    def x8(x3: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      val x7 = x3(x6)
      x7
    }
    def x23(x12: Int): Int = {
      def x17(x15: Int): Int = {
        val x16 = x15 + x12
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
    def x25(x10: Int): Int = {
      val x24 = x8(x23, x10)
      x24
    }
    val x26 = x8(x25, x2)
    x26
  }
}
// output:
