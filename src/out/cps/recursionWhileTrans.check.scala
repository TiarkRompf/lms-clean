class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    lazy val x4 = x5 _
    def x5(x6: Int => Int, x8: Int): Int = {
      val x9 = x8 > 0
      def x13(x11: Int): Int = {
        val x12 = x6(x11)
        x12
      }
      val x24 = if (x9) {
        val x15 = x8 - x1
        def x20(x17: Int): Int = {
          val x18 = x8 * x17
          val x19 = x13(x18)
          x19
        }
        val x21 = x4(x20, x15)
        x21
      } else {
        val x23 = x13(1)
        x23
      }
      x24
    }
    def x25(): Int = {
      val x27 = x2
      val x28 = x27 < 10
      val x43 = if (x28) {
        val x30 = x2
        def x38(x32: Int): Int = {
          x3 = x32
          val x34 = x2
          val x35 = x34 + 1
          x2 = x35
          val x37 = x25()
          x37
        }
        val x39 = x5(x38, x30)
        x39
      } else {
        val x41 = x3
        x41 /*exit: x41 */
      }
      x43
    }
    val x44 = x25()
    x44
  }
}
// output:
