class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x30 = {
      var x3 = 0
      var x4 = 0
      def x16(x11: Int): Int = {
        val x12 = x3
        val x13 = x12 + 1
        x3 = x13
        val x15 = x5()
        x15
      }
      def x5(): Int = {
        val x7 = x3
        val x8 = x7 < 5
        val x28 = if (x8) {
          val x17 = x4
          val x18 = x16(x17)
          val x19 = x16(x1)
          val x20 = x18 + x19
          val x21 = x3
          val x22 = x16(x21)
          val x23 = x20 + x22
          x4 = x23
          val x25 = x4
          x25
        } else {
          val x27 = x4
          x27
        }
        x28
      }
      val x29 = x5()
      x29
    }
    val x31 = x30 + 4
    x31 /*exit: x31 */
  }
}
// output:
