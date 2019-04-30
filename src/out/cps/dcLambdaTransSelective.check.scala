class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x6(x4: Int): Int = {
      val x5 = x4 * 2
      x5
    }
    val x16 = {
      def x11(x9: Int): Int = {
        val x10 = x9 + 2
        x10
      }
      val x12 = x11(x1)
      val x13 = x6(x12)
      val x14 = x11(x13)
      val x15 = x6(x14)
      x15
    }
    val x17 = x16 + 5
    exit(x17)
  }
}
// output:
