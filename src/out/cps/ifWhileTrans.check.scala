class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 + x1
    val x3 = x2 > 10
    def x7(x5: Int): Int = {
      x5 /*exit: x5 */
    }
    val x52 = if (x3) {
      var x9 = 0
      var x10 = 0
      val x11 = x2 - 10
      def x12(): Int = {
        val x14 = x9
        val x15 = x14 < x11
        val x28 = if (x15) {
          val x17 = x10
          val x18 = x9
          val x19 = x17 + x18
          x10 = x19
          val x21 = x9
          val x22 = x21 + 1
          x9 = x22
          val x24 = x12()
          x24
        } else {
          val x26 = x10
          val x27 = x7(x26)
          x27
        }
        x28
      }
      val x29 = x12()
      x29
    } else {
      var x31 = 0
      var x32 = 0
      val x33 = x2 + 10
      def x34(): Int = {
        val x36 = x31
        val x37 = x36 < x33
        val x50 = if (x37) {
          val x39 = x32
          val x40 = x31
          val x41 = x39 + x40
          x32 = x41
          val x43 = x31
          val x44 = x43 + 1
          x31 = x44
          val x46 = x34()
          x46
        } else {
          val x48 = x32
          val x49 = x7(x48)
          x49
        }
        x50
      }
      val x51 = x34()
      x51
    }
    x52
  }
}
// output:
