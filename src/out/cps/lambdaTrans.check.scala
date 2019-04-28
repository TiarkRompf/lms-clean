class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x8(x3: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      val x7 = x3(x6)
      x7
    }
    def x16(x10: Int): Int = {
      def x14(x12: Int): Int = {
        exit(x12)
      }
      val x15 = x8(x14, x10)
      x15
    }
    val x17 = x8(x16, x1)
    x17
  }
}
// output:
