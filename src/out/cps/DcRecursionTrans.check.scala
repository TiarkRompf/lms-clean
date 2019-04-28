class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x36 = {
      def x8(x5: Int): Int = {
        val x6 = x5 * 2
        val x7 = x6 + 3
        x7
      }
      lazy val x9 = x10 _
      def x10(x11: Int => Int, x13: Int): Int = {
        val x14 = x13 > 0
        def x18(x16: Int): Int = {
          val x17 = x11(x16)
          x17
        }
        val x31 = if (x14) {
          val x20 = x8(x13)
          val x21 = x13 - 1
          def x26(x23: Int): Int = {
            val x24 = x20 * x23
            val x25 = x18(x24)
            x25
          }
          val x27 = x9(x26, x21)
          x27
        } else {
          val x29 = x8(x13)
          val x30 = x18(x29)
          x30
        }
        x31
      }
      def x34(x33: Int): Int = {
        x33
      }
      val x35 = x10(x34, x1)
      x35
    }
    val x37 = x36 + 10
    exit(x37)
  }
}
// output:
