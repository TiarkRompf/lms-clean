class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = return res
    val x3 = x1 + x1
    var x4 = 0
    var x5 = 0
    val x6 = x3 > 10
    def x10(x8: Int): Int = {
      exit(x8)
    }
    val x51 = if (x6) {
      val x12 = x3 - 10
      def x13(): Int = {
        val x15 = x4
        val x16 = x15 < x12
        val x29 = if (x16) {
          val x18 = x5
          val x19 = x4
          val x20 = x18 + x19
          x5 = x20
          val x22 = x4
          val x23 = x22 + 1
          x4 = x23
          val x25 = x13()
          x25
        } else {
          val x27 = x5
          val x28 = x10(x27)
          x28
        }
        x29
      }
      val x30 = x13()
      x30
    } else {
      val x32 = x3 + 10
      def x33(): Int = {
        val x35 = x4
        val x36 = x35 < x32
        val x49 = if (x36) {
          val x38 = x5
          val x39 = x4
          val x40 = x38 + x39
          x5 = x40
          val x42 = x4
          val x43 = x42 + 1
          x4 = x43
          val x45 = x33()
          x45
        } else {
          val x47 = x5
          val x48 = x10(x47)
          x48
        }
        x49
      }
      val x50 = x33()
      x50
    }
    x51
  }
}
// output:
