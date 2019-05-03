class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    def x5(): Int = {
      val x7 = x2
      val x8 = x7 < 5
      val x27 = if (x8) {
        val x10 = x3
        val x11 = x2
        val x12 = x10 + x11
        x3 = x12
        val x14 = x2
        val x15 = x14 + 1
        x2 = x15
        val x17 = x5()
        x17
      } else {
        val x19 = x3
        def x24(x21: Int): Int = {
          val x22 = x19 * x21
          val x23 = x22 * 2
          x23
        }
        val x25 = x24(x1)
        val x26 = x24(x25)
        x26
      }
      x27
    }
    val x29 = {
      val x28 = x5()
      x28
    }
    val x30 = x29 + 4
    x30 /*exit: x30 */
  }
}
// output:
