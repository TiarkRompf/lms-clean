class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x12(x9: Int): Int = {
      val x10 = x9 * 2
      val x11 = x10 + 3
      x11
    }
    def x3(x4: Int => Int, x6: Int): Int = {
      val x13 = x6 > 0
      val x18 = x12(x6)
      val x19 = x6 - 1
      val x29 = {
        def x16(x15: Int): Int = {
          x15
        }
        val x28 = if (x13) {
          def x24(x21: Int): Int = {
            val x22 = x18 * x21
            val x23 = x16(x22)
            x23
          }
          val x25 = x2(x24, x19)
          x25
        } else {
          val x27 = x16(x18)
          x27
        }
        x28
      }
      val x30 = x29 + 10
      val x31 = x4(x30)
      x31
    }
    def x35(x33: Int): Int = {
      x33 /*exit: x33 */
    }
    val x36 = x3(x35, x1)
    x36
  }
}
// output:
