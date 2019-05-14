class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x9(x7: Int): Int = {
      val x8 = x7 * 2
      x8
    }
    val x11 = x9(x1)
    def x16(x2: Int => Int, x4: Int): Int = {
      val x10 = x9(x4)
      val x12 = x10 + x11
      val x13 = {
        x12
      }
      val x14 = x13 + 5
      val x15 = x2(x14)
      x15
    }
    val x17 = x1 + 2
    def x21(x19: Int): Int = {
      x19 /*exit: x19 */
    }
    val x22 = x16(x21, x17)
    x22
  }
}
// output:
