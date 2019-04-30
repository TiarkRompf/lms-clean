class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    val x17 = if (x3) {
      val x13 = {
        def x9(x7: Int): Int = {
          val x8 = x7 * 2
          x8
        }
        val x10 = x9(x1)
        val x11 = x9(x10)
        val x12 = x11 + 3
        x12
      }
      val x14 = x13 + 10
      x14
    } else {
      val x16 = x1 + 5
      x16
    }
    exit(x17)
  }
}
// output:
