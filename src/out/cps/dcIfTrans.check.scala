class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    val x23 = {
      def x13(x11: Int): Int = {
        val x12 = 3 + x11
        x12
      }
      val x22 = if (x3) {
        def x9(x6: Int): Int = {
          val x7 = x6 * 2
          val x8 = 1 + x7
          x8
        }
        val x15 = x9(x1)
        val x16 = x9(x15)
        val x17 = x13(x16)
        x17
      } else {
        def x9(x6: Int): Int = {
          val x7 = x6 * 2
          val x8 = 1 + x7
          x8
        }
        val x19 = x9(x1)
        val x20 = x19 + 5
        val x21 = x13(x20)
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
