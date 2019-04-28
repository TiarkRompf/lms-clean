class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    val x4 = x1 - 5
    val x5 = x1 + 100
    val x6 = x5 + 2
    val x23 = {
      def x10(x9: Int): Int = {
        x9
      }
      val x22 = if (x3) {
        def x16(x13: Int): Int = {
          val x14 = x13 * 3
          val x15 = x10(x14)
          x15
        }
        val x17 = x16(x4)
        val x18 = x16(x17)
        val x19 = x18 + 7
        x19
      } else {
        val x21 = x10(x6)
        x21
      }
      x22
    }
    val x24 = x23 + 4
    val x25 = x24 + 10
    exit(x25)
  }
}
// output:
