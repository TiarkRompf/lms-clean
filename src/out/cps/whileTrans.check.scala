class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = return res
    val x3 = x1 + x1
    var x4 = x3
    var x5 = 0
    def x6(): Int = {
      val x8 = x4
      val x9 = x8 > 0
      val x22 = if (x9) {
        val x11 = x5
        val x12 = x4
        val x13 = x11 + x12
        x5 = x13
        val x15 = x4
        val x16 = x15 - 1
        x4 = x16
        val x18 = x6()
        x18
      } else {
        val x20 = x5
        exit(x20)
      }
      x22
    }
    val x23 = x6()
    x23
  }
}
// output:
