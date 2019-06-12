class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    def x15(x13: Int): Int = {
      val x14 = x13 * 2
      x14
    }
    while ({
      val x5 = x2
      val x6 = x5 < 5
      x6
    }) {
      val x8 = x2
      val x9 = x8 + 1
      x2 = x9
      val x24 = {
        val x16 = x3
        val x17 = x15(x16)
        val x18 = x15(x1)
        val x19 = x17 + x18
        val x20 = x2
        val x21 = x15(x20)
        val x22 = x19 + x21
        x3 = x22
        x22
      }
      ()
    }
    val x26 = x3
    x26 /*exit: x26 */
  }
}
// output:
