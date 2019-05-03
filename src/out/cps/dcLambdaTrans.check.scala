class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x2: Int => Int, x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = x2(x5)
      x6
    }
    def x12(x10: Int): Int = {
      val x11 = x10 + 2
      x11
    }
    val x23 = {
      val x13 = x12(x1)
      def x21(x15: Int): Int = {
        val x16 = x12(x15)
        def x19(x18: Int): Int = {
          x18
        }
        val x20 = x7(x19, x16)
        x20
      }
      val x22 = x7(x21, x13)
      x22
    }
    val x24 = x23 + 5
    x24 /*exit: x24 */
  }
}
// output:
