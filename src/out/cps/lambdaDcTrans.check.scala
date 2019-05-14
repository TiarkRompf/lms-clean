class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x13(x3: Int => Int, x5: Int): Int = {
      def x9(x7: Int): Int = {
        val x8 = x3(x7)
        x8
      }
      val x10 = x9(x5)
      val x11 = x9(x1)
      val x12 = x10 + x11
      x12
    }
    val x14 = x1 + 2
    val x19 = {
      def x17(x16: Int): Int = {
        x16
      }
      val x18 = x13(x17, x14)
      x18
    }
    val x20 = x19 + 5
    x20 /*exit: x20 */
  }
}
// output:
