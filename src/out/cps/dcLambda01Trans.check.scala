class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x8(x3: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      val x7 = x3(x6)
      x7
    }
    val x21 = {
      def x18(x11: Int): Int = {
        val x12 = x11 + 2
        def x16(x14: Int): Int = {
          val x15 = x14 + 3
          x15
        }
        val x17 = x8(x16, x12)
        x17
      }
      val x19 = x18(x1)
      val x20 = x18(x19)
      x20
    }
    val x22 = x21 + 5
    exit(x22)
  }
}
// output:
