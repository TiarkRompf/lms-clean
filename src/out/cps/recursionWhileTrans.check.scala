class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = return res
    var x3 = 0
    var x4 = 0
    lazy val x5 = x6 _
    def x6(x7: Int => Int, x9: Int): Int = {
      val x10 = x9 > 0
      def x14(x12: Int): Int = {
        val x13 = x7(x12)
        x13
      }
      val x25 = if (x10) {
        val x16 = x9 - x1
        def x21(x18: Int): Int = {
          val x19 = x9 * x18
          val x20 = x14(x19)
          x20
        }
        val x22 = x5(x21, x16)
        x22
      } else {
        val x24 = x14(1)
        x24
      }
      x25
    }
    def x26(): Int = {
      val x28 = x3
      val x29 = x28 < 10
      val x44 = if (x29) {
        val x31 = x3
        def x39(x33: Int): Int = {
          x4 = x33
          val x35 = x3
          val x36 = x35 + 1
          x3 = x36
          val x38 = x26()
          x38
        }
        val x40 = x6(x39, x31)
        x40
      } else {
        val x42 = x4
        exit(x42)
      }
      x44
    }
    val x45 = x26()
    x45
  }
}
// output:
