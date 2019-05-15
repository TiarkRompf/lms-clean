class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x6(x4: Int): Int = {
      val x5 = x4 * 2
      x5
    }
    val x30 = {
      var x7 = 0
      var x8 = 0
      def x9(): Int = {
        val x11 = x7
        val x12 = x11 < 5
        val x28 = if (x12) {
          val x14 = x8
          val x15 = x6(x14)
          val x16 = x6(x1)
          val x17 = x15 + x16
          val x18 = x7
          val x19 = x6(x18)
          val x20 = x17 + x19
          x8 = x20
          val x22 = x7
          val x23 = x22 + 1
          x7 = x23
          val x25 = x9()
          x25
        } else {
          val x27 = x8
          x27
        }
        x28
      }
      val x29 = x9()
      x29
    }
    val x31 = x30 + 4
    x31 /*exit: x31 */
  }
}
// output:
