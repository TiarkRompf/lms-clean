class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x6(x4: Int): Int = {
      val x5 = x4 + 4
      x5
    }
    val x7 = x6(x1)
    val x8 = 2 * x7
    def x13(x11: Int): Int = {
      val x12 = x11 + 10
      x12
    }
    val x14 = x13(x1)
    val x15 = x13(x14)
    def x21(x18: Int): Int = {
      val x19 = x18 * 2
      val x20 = 11 + x19
      x20
    }
    val x22 = x21(x1)
    val x23 = x21(14)
    val x24 = x22 + x23
    val x29 = {
      val x27 = {
        val x25 = {
          x24
        }
        val x26 = x15 + x25
        x26
      }
      val x28 = x8 * x27
      x28
    }
    val x30 = x29 * x1
    x30 /*exit: x30 */
  }
}
// output:
