class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    lazy val x3 = x4 _
    def x4(x6: Int): Int = {
      val x7 = x6 > 0
      val x8 = x6 - 1
      val x22 = {
        def x14(x11: Int): Int = {
          val x12 = x11 * 2
          val x13 = x12 + 3
          x13
        }
        val x21 = if (x7) {
          val x16 = x14(x6)
          val x17 = x3(x8)
          val x18 = x16 * x17
          x18
        } else {
          val x20 = x14(x6)
          x20
        }
        x21
      }
      val x23 = x22 + 10
      x23
    }
    val x24 = x4(x1)
    exit(x24)
  }
}
// output:
