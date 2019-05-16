class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x12(x2: Int => Int, x4: Int): Int = {
      def x8(x6: Int): Int = {
        val x7 = x2(x6)
        x7
      }
      val x9 = x8(x4)
      val x10 = x8(x1)
      val x11 = x9 + x10
      x11
    }
    val x13 = x1 + 2
    val x19 = {
      def x17(x16: Int): Int = {
        x16
      }
      val x18 = x12(x17, x13)
      x18
    }
    val x20 = x19 + 5
    x20 /*exit: x20 */
  }
}
// output:
