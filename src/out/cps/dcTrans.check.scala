class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x16 = {
      def x13(x5: Int): Int = {
        def x10(x7: Int): Int = {
          val x8 = x5 + x7
          val x9 = x8 + 4
          x9
        }
        val x11 = x10(x1)
        val x12 = 3 * x11
        x12
      }
      val x14 = x13(x1)
      val x15 = 2 * x14
      x15
    }
    val x17 = x16 * x1
    exit(x17)
  }
}
// output:
