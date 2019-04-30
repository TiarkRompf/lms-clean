class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x15(x4: Int): Int = {
      val x13 = {
        def x9(x7: Int): Int = {
          val x8 = x7 * 2
          x8
        }
        val x10 = x9(x4)
        val x11 = x9(x1)
        val x12 = x10 + x11
        x12
      }
      val x14 = x13 + 5
      x14
    }
    val x16 = x1 + 2
    val x17 = x15(x16)
    exit(x17)
  }
}
// output:
