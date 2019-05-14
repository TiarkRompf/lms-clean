class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x2: Int => Int, x4: Int): Int = {
      val x5 = x4 * 2
      val x6 = x2(x5)
      x6
    }
    def x13(x11: Int): Int = {
      x11 /*exit: x11 */
    }
    def x15(x9: Int): Int = {
      val x14 = x7(x13, x9)
      x14
    }
    val x16 = x7(x15, x1)
    x16
  }
}
// output:
