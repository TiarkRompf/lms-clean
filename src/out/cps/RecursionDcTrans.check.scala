class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x14(x11: Int): Int = {
      val x12 = x11 * 2
      val x13 = x12 + 3
      x13
    }
    def x3(x4: Int => Int, x6: Int): Int = {
      val x7 = x6 > 0
      val x8 = x6 - 1
      val x30 = {
        def x17(x16: Int): Int = {
          x16
        }
        val x29 = if (x7) {
          val x19 = x14(x6)
          def x24(x21: Int): Int = {
            val x22 = x19 * x21
            val x23 = x17(x22)
            x23
          }
          val x25 = x2(x24, x8)
          x25
        } else {
          val x27 = x14(x6)
          val x28 = x17(x27)
          x28
        }
        x29
      }
      val x31 = x30 + 10
      val x32 = x4(x31)
      x32
    }
    def x36(x34: Int): Int = {
      x34 /*exit: x34 */
    }
    val x37 = x3(x36, x1)
    x37
  }
}
// output:
