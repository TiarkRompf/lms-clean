class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x27 = {
      def x7(x5: Int): Int = {
        val x6 = x5 * 2
        x6
      }
      var x8 = 0
      var x9 = 0
      while ({
        val x11 = x8
        val x12 = x11 < 5
        x12
      }) {
        val x14 = x9
        val x15 = x7(x14)
        val x16 = x7(x1)
        val x17 = x15 + x16
        val x18 = x8
        val x19 = x7(x18)
        val x20 = x17 + x19
        x9 = x20
        val x22 = x8
        val x23 = x22 + 1
        x8 = x23
        ()
      }
      val x26 = x9
      x26
    }
    val x28 = x27 + 4
    exit(x28)
  }
}
// output:
