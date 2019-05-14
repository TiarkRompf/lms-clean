class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x6(x4: Int): Int = {
      val x5 = x4 * 2
      x5
    }
    val x9 = x6(x1)
    val x26 = {
      var x7 = 0
      var x8 = 0
      while ({
        val x11 = x7
        val x12 = x11 < 5
        x12
      }) {
        val x14 = x8
        val x15 = x6(x14)
        val x16 = x15 + x9
        val x17 = x7
        val x18 = x6(x17)
        val x19 = x16 + x18
        x8 = x19
        val x21 = x7
        val x22 = x21 + 1
        x7 = x22
        ()
      }
      val x25 = x8
      x25
    }
    val x27 = x26 + 4
    x27 /*exit: x27 */
  }
}
// output:
