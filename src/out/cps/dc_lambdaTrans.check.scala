class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 * 2
    def x9(x4: Int => Int, x6: Int): Int = {
      val x7 = x6 * 2
      val x8 = x4(x7)
      x8
    }
    val x27 = {
      var x11 = x3
      def x24(x13: Int): Int = {
        val x14 = x11
        def x22(x16: Int): Int = {
          def x20(x18: Int): Int = {
            val x19 = x13 + x18
            x19
          }
          val x21 = x9(x20, x16)
          x21
        }
        val x23 = x9(x22, x14)
        x23
      }
      val x25 = x24(x1)
      val x26 = x24(x25)
      x26
    }
    val x28 = x27 + 5
    exit(x28)
  }
}
// output:
