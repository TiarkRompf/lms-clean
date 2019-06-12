class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    def x6(x4: Int): Int = {
      val x5 = x4 * 2
      x5
    }
    val x7 = x6(x2)
    val x8 = x6(x7)
    def x13(x11: Int): Int = {
      val x12 = x11 + x8
      x12
    }
    val x16 = {
      val x14 = x13(x1)
      val x15 = x13(x14)
      x15
    }
    val x17 = x16 + 5
    x17 /*exit: x17 */
  }
}
// output:
