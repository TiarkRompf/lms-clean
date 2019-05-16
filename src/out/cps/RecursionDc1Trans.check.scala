class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x3(x4: Int => Int, x6: Int): Int = {
      val x7 = x6 > 0
      def x11(x9: Int): Int = {
        val x10 = x4(x9)
        x10
      }
      val x27 = if (x7) {
        val x15 = x6 - 1
        def x22(x14: Int): Int = {
          def x20(x17: Int): Int = {
            val x18 = x14 + x17
            val x19 = x11(x18)
            x19
          }
          val x21 = x2(x20, x15)
          x21
        }
        val x23 = x22(x6)
        val x24 = x22(x23)
        x24
      } else {
        val x26 = x11(1)
        x26
      }
      x27
    }
    val x33 = {
      def x31(x30: Int): Int = {
        x30
      }
      val x32 = x3(x31, x1)
      x32
    }
    x33 /*exit: x33 */
  }
}
// output:
