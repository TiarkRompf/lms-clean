class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 * 2
    def x7(x5: Int): Int = {
      val x6 = x5 * 2
      x6
    }
    val x19 = {
      var x9 = x3
      def x16(x11: Int): Int = {
        val x12 = x9
        val x13 = x7(x12)
        val x14 = x7(x13)
        val x15 = x11 + x14
        x15
      }
      val x17 = x16(x1)
      val x18 = x16(x17)
      x18
    }
    val x20 = x19 + 5
    exit(x20)
  }
}
// output:
