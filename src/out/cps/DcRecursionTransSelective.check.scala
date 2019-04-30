class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x23 = {
      def x8(x5: Int): Int = {
        val x6 = x5 * 2
        val x7 = x6 + 3
        x7
      }
      lazy val x9 = x10 _
      def x10(x12: Int): Int = {
        val x13 = x12 > 0
        val x21 = if (x13) {
          val x15 = x8(x12)
          val x16 = x12 - 1
          val x17 = x9(x16)
          val x18 = x15 * x17
          x18
        } else {
          val x20 = x8(x12)
          x20
        }
        x21
      }
      val x22 = x10(x1)
      x22
    }
    val x24 = x23 + 10
    exit(x24)
  }
}
// output:
