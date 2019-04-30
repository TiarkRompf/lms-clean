class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    val x18 = {
      def x9(x6: Int): Int = {
        val x7 = x6 * 2
        val x8 = 1 + x7
        x8
      }
      val x16 = if (x3) {
        val x11 = x9(x1)
        val x12 = x9(x11)
        x12
      } else {
        val x14 = x9(x1)
        val x15 = x14 + 5
        x15
      }
      val x17 = 3 + x16
      x17
    }
    val x19 = x18 + 4
    val x20 = x19 + 10
    exit(x20)
  }
}
// output:
