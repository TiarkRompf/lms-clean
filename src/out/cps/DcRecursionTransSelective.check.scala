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
      val x19 = if (x12) {
        val x14 = x7(x11)
        val x15 = x11 - 1
        val x16 = x8(x15)
        val x17 = x14 * x16
        x17
      } else {
        val x14 = x7(x11)
        x14
      }
      x19
    }
    val x20 = x9(x1)
    val x21 = {
      x20
    }
    val x22 = x21 + 10
    x22 /*exit: x22 */
  }
}
// output:
