class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x17 = {
      def x4(x14: Int) = {
        val x15 = x14 * 2
        val x16 = 1 + x15
        x16
      }
      val x5 = x1 > 5
      def cIf0(x12: Int) = {
        val x13 = 3 + x12
        x13
      }
      if (x5) {
        val x7 = x4(x1)
        val x8 = x4(x7)
        cIf0(x8)
      } else {
        val x10 = x4(x1)
        val x11 = x10 + 5
        cIf0(x11)
      }
    }
    val x18 = x17 + 4
    val x19 = x18 + 10
    x19 /*exit x19*/
  }
}
// output:
