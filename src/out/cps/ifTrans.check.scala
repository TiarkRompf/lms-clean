class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = return res
    val x3 = x1 + x1
    val x4 = x3 > 2
    def x8(x6: Int): Int = {
      exit(x6)
    }
    val x17 = if (x4) {
      val x10 = x3 * x1
      val x11 = x10 + x1
      val x12 = x8(x11)
      x12
    } else {
      val x14 = x3 / x1
      val x15 = x14 - x1
      val x16 = x8(x15)
      x16
    }
    x17
  }
}
// output:
