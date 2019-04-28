class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    lazy val x3 = x4 _
    def x4(x5: Int => Int, x7: Int): Int = {
      val x8 = x7 > 0
      def x12(x10: Int): Int = {
        val x11 = x5(x10)
        x11
      }
      val x23 = if (x8) {
        val x14 = x7 - 1
        def x19(x16: Int): Int = {
          val x17 = x7 * x16
          val x18 = x12(x17)
          x18
        }
        val x20 = x3(x19, x14)
        x20
      } else {
        val x22 = x12(1)
        x22
      }
      x23
    }
    def x27(x25: Int): Int = {
      exit(x25)
    }
    val x28 = x4(x27, x1)
    x28
  }
}
// output:
