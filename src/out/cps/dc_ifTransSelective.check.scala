class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 * 2
    val x4 = x1 - 5
    val x5 = x1 + 5
    val x20 = {
      var x7 = x3
      def x16(x9: Int): Int = {
        val x10 = x7
        val x11 = x10 > 5
        val x14 = if (x11) {
          x4
        } else {
          x5
        }
        val x15 = x9 * x14
        x15
      }
      val x17 = x16(x1)
      val x18 = x16(x17)
      val x19 = x18 + 3
      x19
    }
    val x21 = x20 + 10
    exit(x21)
  }
}
// output:
