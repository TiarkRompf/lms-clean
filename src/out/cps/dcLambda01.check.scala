class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x3(c: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      c(x6)
    }
    val x16 = {
      def x9(x12: Int) = {
        val x13 = x12 + 2
        def cApp0(x14: Int) = {
          val x15 = x14 + 3
          x15
        }
        x3(cApp0, x13)
      }
      val x10 = x9(x1)
      val x11 = x9(x10)
      x11
    }
    val x17 = x16 + 5
    exit(x17)
  }
}
// output:
