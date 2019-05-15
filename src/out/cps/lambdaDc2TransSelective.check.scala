class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x8(x6: Int): Int = {
      val x7 = x6 * 2
      x7
    }
    def x14(x3: Int): Int = {
      val x12 = {
        val x9 = x8(x3)
        val x10 = x8(x1)
        val x11 = x9 + x10
        x11
      }
      val x13 = x12 + 5
      x13
    }
    val x15 = x1 + 2
    val x16 = x14(x15)
    x16 /*exit: x16 */
  }
}
// output:
