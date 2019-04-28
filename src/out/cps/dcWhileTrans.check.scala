class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x31 = {
      def x7(x5: Int): Int = {
        val x6 = x5 * 2
        x6
      }
      var x8 = 0
      var x9 = 0
      def x10(): Int = {
        val x12 = x8
        val x13 = x12 < 5
        val x29 = if (x13) {
          val x15 = x9
          val x16 = x7(x15)
          val x17 = x7(x1)
          val x18 = x16 + x17
          val x19 = x8
          val x20 = x7(x19)
          val x21 = x18 + x20
          x9 = x21
          val x23 = x8
          val x24 = x23 + 1
          x8 = x24
          val x26 = x10()
          x26
        } else {
          val x28 = x9
          x28
        }
        x29
      }
      val x30 = x10()
      x30
    }
    val x32 = x31 + 4
    exit(x32)
  }
}
// output:
