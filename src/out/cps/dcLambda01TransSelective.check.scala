class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x5(x3: Int): Int = {
      val x4 = x3 * 2
      x4
    }
    def x12(x8: Int): Int = {
      val x9 = x8 + 2
      val x10 = x5(x9)
      val x11 = x10 + 3
      x11
    }
    val x15 = {
      val x13 = x12(x1)
      val x14 = x12(x13)
      x14
    }
    val x16 = x15 + 5
    x16 /*exit: x16 */
  }
}
// output:
