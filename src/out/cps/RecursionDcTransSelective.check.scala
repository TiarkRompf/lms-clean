class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x13(x10: Int): Int = {
      val x11 = x10 * 2
      val x12 = x11 + 3
      x12
    }
    def x3(x5: Int): Int = {
      val x6 = x5 > 0
      val x7 = x5 - 1
      val x21 = {
        val x20 = if (x6) {
          val x15 = x13(x5)
          val x16 = x2(x7)
          val x17 = x15 * x16
          x17
        } else {
          val x19 = x13(x5)
          x19
        }
        x20
      }
      val x22 = x21 + 10
      x22
    }
    val x23 = x3(x1)
    x23 /*exit: x23 */
  }
}
// output:
