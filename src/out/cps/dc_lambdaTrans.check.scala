class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    def x10(x5: Int => Int, x7: Int): Int = {
      val x8 = x7 * 2
      val x9 = x5(x8)
      x9
    }
    val x26 = {
      var x4 = x3
      def x23(x12: Int): Int = {
        val x13 = x4
        def x19(x17: Int): Int = {
          val x18 = x12 + x17
          x18
        }
        def x21(x15: Int): Int = {
          val x20 = x10(x19, x15)
          x20
        }
        val x22 = x10(x21, x13)
        x22
      }
      val x24 = x23(x1)
      val x25 = x23(x24)
      x25
    }
    val x27 = x26 + 5
    x27 /*exit: x27 */
  }
}
// output:
