class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x11(x8: Int): Int = {
      val x9 = x8 * 2
      val x10 = x9 + 3
      x10
    }
    def x3(x5: Int): Int = {
      val x12 = x5 > 0
      val x19 = if (x12) {
        val x14 = x11(x5)
        val x15 = x5 - 1
        val x16 = x2(x15)
        val x17 = x14 * x16
        x17
      } else {
        val x14 = x11(x5)
        x14
      }
      val x20 = {
        x19
      }
      val x21 = x20 + 10
      x21
    }
    val x22 = x3(x1)
    x22 /*exit: x22 */
  }
}
// output:
