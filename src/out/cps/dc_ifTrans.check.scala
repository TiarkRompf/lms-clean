class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 * 2
    val x4 = x1 - 5
    val x5 = x1 + 5
    val x25 = {
      var x7 = x3
      def x21(x9: Int): Int = {
        val x10 = x7
        val x11 = x10 > 5
        def x15(x13: Int): Int = {
          val x14 = x9 * x13
          x14
        }
        val x20 = if (x11) {
          val x17 = x15(x4)
          x17
        } else {
          val x19 = x15(x5)
          x19
        }
        x20
      }
      val x22 = x21(x1)
      val x23 = x21(x22)
      val x24 = x23 + 3
      x24
    }
    val x26 = x25 + 10
    exit(x26)
  }
}
// output:
