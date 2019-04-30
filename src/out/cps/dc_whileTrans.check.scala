class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    var x3 = 0
    var x4 = 0
    def x6(): Int = {
      val x8 = x3
      val x9 = x8 < 5
      val x28 = if (x9) {
        val x11 = x4
        val x12 = x3
        val x13 = x11 + x12
        x4 = x13
        val x15 = x3
        val x16 = x15 + 1
        x3 = x16
        val x18 = x6()
        x18
      } else {
        val x20 = x4
        def x25(x22: Int): Int = {
          val x23 = x20 * x22
          val x24 = x23 * 2
          x24
        }
        val x26 = x25(x1)
        val x27 = x25(x26)
        x27
      }
      x28
    }
    val x30 = {
      val x29 = x6()
      x29
    }
    val x31 = x30 + 4
    exit(x31)
  }
}
// output:
