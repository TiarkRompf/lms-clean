class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    def x16(x14: Int): Int = {
      val x15 = x14 * 2
      x15
    }
    def x4(): Int = {
      val x6 = x2
      val x7 = x6 < 5
      val x30 = if (x7) {
        val x9 = x2
        val x10 = x9 + 1
        x2 = x10
        val x25 = {
          val x17 = x3
          val x18 = x16(x17)
          val x19 = x16(x1)
          val x20 = x18 + x19
          val x21 = x2
          val x22 = x16(x21)
          val x23 = x20 + x22
          x3 = x23
          x23
        }
        val x26 = x4()
        x26
      } else {
        val x28 = x3
        x28 /*exit: x28 */
      }
      x30
    }
    val x31 = x4()
    x31
  }
}
// output:
