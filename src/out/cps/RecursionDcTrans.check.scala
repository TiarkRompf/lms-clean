class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    lazy val x3 = x4 _
    def x4(x5: Int => Int, x7: Int): Int = {
      val x8 = x7 > 0
      val x9 = x7 - 1
      val x31 = {
        def x18(x17: Int): Int = {
          x17
        }
        val x30 = if (x8) {
          def x15(x12: Int): Int = {
            val x13 = x12 * 2
            val x14 = x13 + 3
            x14
          }
          val x20 = x15(x7)
          def x25(x22: Int): Int = {
            val x23 = x20 * x22
            val x24 = x18(x23)
            x24
          }
          val x26 = x3(x25, x9)
          x26
        } else {
          def x15(x12: Int): Int = {
            val x13 = x12 * 2
            val x14 = x13 + 3
            x14
          }
          val x28 = x15(x7)
          val x29 = x18(x28)
          x29
        }
        x30
      }
      val x32 = x31 + 10
      val x33 = x5(x32)
      x33
    }
    def x37(x35: Int): Int = {
      exit(x35)
    }
    val x38 = x4(x37, x1)
    x38
  }
}
// output:
