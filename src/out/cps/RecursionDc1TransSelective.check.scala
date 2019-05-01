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
      val x28 = if (x8) {
        val x16 = x7 - 1
        def x23(x15: Int): Int = {
          def x21(x18: Int): Int = {
            val x19 = x15 + x18
            val x20 = x12(x19)
            x20
          }
          val x22 = x3(x21, x16)
          x22
        }
        val x24 = x23(x7)
        val x25 = x23(x24)
        x25
      } else {
        val x27 = x12(1)
        x27
      }
      x28
    }
    val x34 = {
      def x32(x31: Int): Int = {
        x31
      }
      val x33 = x4(x32, x1)
      x33
    }
    exit(x34)
  }
}
// output:
