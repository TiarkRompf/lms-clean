class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x24 = {
      val x3 = x1 > 5
      def cIf0(x23: Int) = {
        x23
      }
      if (x3) {
        def x7(x11: Int) = {
          val x12 = x11 * 3
          cIf0(x12)
        }
        val x5 = x1 - 5
        val x8 = x7(x5)
        val x9 = x7(x8)
        val x10 = x9 + 7
        x10
      } else {
        def x16(x20: Int) = {
          val x21 = x20 * 2
          val x22 = x21 + 2
          cIf0(x22)
        }
        val x14 = x1 + 5
        val x17 = x16(x14)
        val x18 = x16(x17)
        val x19 = x18 + 7
        x19
      }
    }
    val x25 = x24 + 4
    val x26 = x25 + 10
    x26 /*exit x26*/
  }
}
// output:
