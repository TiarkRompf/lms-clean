class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 + x1
    var x3 = 0
    var x4 = 0
    val x5 = x2 > 10
    def x9(x7: Int): Int = {
      x7 /*exit: x7 */
    }
    val x50 = if (x5) {
      val x11 = x2 - 10
      def x12(): Int = {
        val x14 = x3
        val x15 = x14 < x11
        val x28 = if (x15) {
          val x17 = x4
          val x18 = x3
          val x19 = x17 + x18
          x4 = x19
          val x21 = x3
          val x22 = x21 + 1
          x3 = x22
          val x24 = x12()
          x24
        } else {
          val x26 = x4
          val x27 = x9(x26)
          x27
        }
        x28
      }
      val x29 = x12()
      x29
    } else {
      val x31 = x2 + 10
      def x32(): Int = {
        val x34 = x3
        val x35 = x34 < x31
        val x48 = if (x35) {
          val x37 = x4
          val x38 = x3
          val x39 = x37 + x38
          x4 = x39
          val x41 = x3
          val x42 = x41 + 1
          x3 = x42
          val x44 = x32()
          x44
        } else {
          val x46 = x4
          val x47 = x9(x46)
          x47
        }
        x48
      }
      val x49 = x32()
      x49
    }
    x50
  }
}
// output:
