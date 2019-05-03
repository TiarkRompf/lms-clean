class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x4(c: Int => Int, x6: Int): Int = {
      def x8(x12: Int) = {
        c(x12)
      }
      val x9 = x8(x6)
      val x10 = x8(x1)
      val x11 = x9 + x10
      x11
    }
    val x13 = x1 + 2
    val x15 = {
      def cApp0(x14: Int) = {
        x14
      }
      x4(cApp0, x13)
    }
    val x16 = x15 + 5
    x16 /*exit x16*/
  }
}
// output:
