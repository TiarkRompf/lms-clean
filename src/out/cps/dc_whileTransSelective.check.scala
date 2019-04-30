class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    var x3 = 0
    var x4 = 0
    val x26 = {
      while ({
        val x7 = x3
        val x8 = x7 < 5
        x8
      }) {
        val x10 = x4
        val x11 = x3
        val x12 = x10 + x11
        x4 = x12
        val x14 = x3
        val x15 = x14 + 1
        x3 = x15
        ()
      }
      val x18 = x4
      def x23(x20: Int): Int = {
        val x21 = x18 * x20
        val x22 = x21 * 2
        x22
      }
      val x24 = x23(x1)
      val x25 = x23(x24)
      x25
    }
    val x27 = x26 + 4
    exit(x27)
  }
}
// output:
