class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 * 2
    def x8(x3: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      val x7 = x3(x6)
      x7
    }
    val x26 = {
      var x10 = x2
      def x23(x12: Int): Int = {
        val x13 = x10
        def x21(x15: Int): Int = {
          def x19(x17: Int): Int = {
            val x18 = x12 + x17
            x18
          }
          val x20 = x8(x19, x15)
          x20
        }
        val x22 = x8(x21, x13)
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
