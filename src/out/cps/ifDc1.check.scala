class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    val x5 = x1 - 5
    val x14 = x1 + 100
    val x15 = x14 + 2
    val x17 = {
      def cIf0(x16: Int) = {
        x16
      }
      if (x3) {
        def x7(x11: Int) = {
          val x12 = x11 * 3
          cIf0(x12)
        }
        val x8 = x7(x5)
        val x9 = x7(x8)
        val x10 = x9 + 7
        x10
      } else {
        cIf0(x15)
      }
    }
    val x18 = x17 + 4
    val x19 = x18 + 10
    exit(x19)
  }
}
// output:
