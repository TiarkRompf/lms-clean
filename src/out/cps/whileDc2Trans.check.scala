class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    def x16(x14: Int): Int = {
      val x15 = x14 * 2
      x15
    }
    val x19 = x16(x1)
    def x4(): Int = {
      val x6 = x2
      val x7 = x6 < 5
      val x31 = if (x7) {
        val x9 = x2
        val x10 = x9 + 1
        x2 = x10
        val x26 = {
          val x17 = x3
          val x18 = x16(x17)
          val x20 = x18 + x19
          val x21 = x2
          val x22 = x16(x21)
          val x23 = x20 + x22
          x3 = x23
          val x25 = x3
          x25
        }
        val x27 = x4()
        x27
      } else {
        val x29 = x3
        x29 /*exit: x29 */
      }
      x31
    }
    val x32 = x4()
    x32
  }
}
// output:
