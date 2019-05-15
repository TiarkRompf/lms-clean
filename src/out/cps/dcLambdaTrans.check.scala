class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x6(x4: Int): Int = {
      val x5 = x4 + 2
      x5
    }
    def x12(x7: Int => Int, x9: Int): Int = {
      val x10 = x9 * 2
      val x11 = x7(x10)
      x11
    }
    def x19(x18: Int): Int = {
      x18
    }
    def x21(x15: Int): Int = {
      val x16 = x6(x15)
      val x20 = x12(x19, x16)
      x20
    }
    val x23 = {
      val x13 = x6(x1)
      val x22 = x12(x21, x13)
      x22
    }
    val x24 = x23 + 5
    x24 /*exit: x24 */
  }
}
// output:
