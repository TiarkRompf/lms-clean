class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x6(c: Int => Int, x8: Int): Int = {
      val x9 = x8 * 2
      c(x9)
    }
    val x16 = {
      def x4(x14: Int) = {
        val x15 = x14 + 2
        x15
      }
      val x10 = x4(x1)
      def cApp0(x11: Int) = {
        val x12 = x4(x11)
        def cApp1(x13: Int) = {
          x13
        }
        x6(cApp1, x12)
      }
      x6(cApp0, x10)
    }
    val x17 = x16 + 5
    x17 /*exit x17*/
  }
}
// output:
