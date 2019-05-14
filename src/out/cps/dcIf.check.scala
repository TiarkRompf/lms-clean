class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x16 = {
      def x4(x13: Int) = {
        val x14 = x13 * 2
        val x15 = 1 + x14
        x15
      }
      val x5 = x1 > 5
      def cIf0(x11: Int) = {
        val x12 = 3 + x11
        x12
      }
      if (x5) {
        val x7 = x4(x1)
        val x8 = x4(x7)
        cIf0(x8)
      } else {
        val x7 = x4(x1)
        val x10 = x7 + 5
        cIf0(x10)
      }
    }
    val x17 = x16 + 4
    val x18 = x17 + 10
    x18 /*exit x18*/
  }
}
// output:
