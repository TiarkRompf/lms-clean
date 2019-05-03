class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x3(x4: Int => Int, x6: Int): Int = {
      val x7 = x6 > 0
      def x11(x9: Int): Int = {
        val x10 = x4(x9)
        x10
      }
      val x22 = if (x7) {
        val x13 = x6 - 1
        def x18(x15: Int): Int = {
          val x16 = x6 * x15
          val x17 = x11(x16)
          x17
        }
        val x19 = x2(x18, x13)
        x19
      } else {
        val x21 = x11(1)
        x21
      }
      x22
    }
    def x26(x24: Int): Int = {
      x24 /*exit: x24 */
    }
    val x27 = x3(x26, x1)
    x27
  }
}
// output:
