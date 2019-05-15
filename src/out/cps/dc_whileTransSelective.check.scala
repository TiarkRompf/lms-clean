class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    val x25 = {
      while ({
        val x6 = x2
        val x7 = x6 < 5
        x7
      }) {
        val x9 = x3
        val x10 = x2
        val x11 = x9 + x10
        x3 = x11
        val x13 = x2
        val x14 = x13 + 1
        x2 = x14
        ()
      }
      val x17 = x3
      def x22(x19: Int): Int = {
        val x20 = x17 * x19
        val x21 = x20 * 2
        x21
      }
      val x23 = x22(x1)
      val x24 = x22(x23)
      x24
    }
    val x26 = x25 + 4
    x26 /*exit: x26 */
  }
}
// output:
