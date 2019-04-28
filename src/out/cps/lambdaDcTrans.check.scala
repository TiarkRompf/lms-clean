class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x13(x3: Int => Int, x5: Int): Int = {
      def x9(x7: Int): Int = {
        val x8 = x3(x7)
        x8
      }
      val x10 = x9(x5)
      val x11 = x9(x1)
      val x12 = x10 + x11
      x12
    }
    val x14 = x1 + 2
    val x20 = {
      def x18(x17: Int): Int = {
        x17
      }
      val x19 = x13(x18, x14)
      x19
    }
    val x21 = x20 + 5
    exit(x21)
  }
}
// output:
