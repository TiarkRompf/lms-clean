class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    val x4 = x1 - 5
    val x5 = x1 + 5
    val x30 = {
      def x9(x8: Int): Int = {
        x8
      }
      val x29 = if (x3) {
        def x15(x12: Int): Int = {
          val x13 = x12 * 3
          val x14 = x9(x13)
          x14
        }
        val x16 = x15(x4)
        val x17 = x15(x16)
        val x18 = x17 + 7
        x18
      } else {
        def x25(x21: Int): Int = {
          val x22 = x21 * 2
          val x23 = x22 + 2
          val x24 = x9(x23)
          x24
        }
        val x26 = x25(x5)
        val x27 = x25(x26)
        val x28 = x27 + 7
        x28
      }
      x29
    }
    val x31 = x30 + 4
    val x32 = x31 + 10
    exit(x32)
  }
}
// output:
