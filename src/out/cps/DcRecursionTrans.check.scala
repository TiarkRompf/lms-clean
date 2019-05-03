class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = x5 + 3
      x6
    }
    lazy val x8 = x9 _
    def x9(x10: Int => Int, x12: Int): Int = {
      val x13 = x12 > 0
      def x17(x15: Int): Int = {
        val x16 = x10(x15)
        x16
      }
      val x30 = if (x13) {
        val x19 = x7(x12)
        val x20 = x12 - 1
        def x25(x22: Int): Int = {
          val x23 = x19 * x22
          val x24 = x17(x23)
          x24
        }
        val x26 = x8(x25, x20)
        x26
      } else {
        val x28 = x7(x12)
        val x29 = x17(x28)
        x29
      }
      x30
    }
    val x35 = {
      def x33(x32: Int): Int = {
        x32
      }
      val x34 = x9(x33, x1)
      x34
    }
    val x36 = x35 + 10
    x36 /*exit: x36 */
  }
}
// output:
