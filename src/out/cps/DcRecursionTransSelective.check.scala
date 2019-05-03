class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = x5 + 3
      x6
    }
    lazy val x8 = x9 _
    def x9(x11: Int): Int = {
      val x12 = x11 > 0
      val x20 = if (x12) {
        val x14 = x7(x11)
        val x15 = x11 - 1
        val x16 = x8(x15)
        val x17 = x14 * x16
        x17
      } else {
        val x19 = x7(x11)
        x19
      }
      x20
    }
    val x22 = {
      val x21 = x9(x1)
      x21
    }
    val x23 = x22 + 10
    x23 /*exit: x23 */
  }
}
// output:
