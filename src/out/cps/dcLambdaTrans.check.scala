class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x8(x3: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      val x7 = x3(x6)
      x7
    }
    val x24 = {
      def x13(x11: Int): Int = {
        val x12 = x11 + 2
        x12
      }
      val x14 = x13(x1)
      def x22(x16: Int): Int = {
        val x17 = x13(x16)
        def x20(x19: Int): Int = {
          x19
        }
        val x21 = x8(x20, x17)
        x21
      }
      val x23 = x8(x22, x14)
      x23
    }
    val x25 = x24 + 5
    exit(x25)
  }
}
// output:
