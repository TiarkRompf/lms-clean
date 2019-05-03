class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 + x1
    var x3 = x2
    var x4 = 0
    def x5(): Int = {
      val x7 = x3
      val x8 = x7 > 0
      val x21 = if (x8) {
        val x10 = x4
        val x11 = x3
        val x12 = x10 + x11
        x4 = x12
        val x14 = x3
        val x15 = x14 - 1
        x3 = x15
        val x17 = x5()
        x17
      } else {
        val x19 = x4
        x19 /*exit: x19 */
      }
      x21
    }
    val x22 = x5()
    x22
  }
}
// output:
