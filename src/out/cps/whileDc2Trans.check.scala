class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    var x3 = 0
    var x4 = 0
    def x5(): Int = {
      val x7 = x3
      val x8 = x7 < 5
      val x32 = if (x8) {
        val x10 = x3
        val x11 = x10 + 1
        x3 = x11
        val x27 = {
          def x17(x15: Int): Int = {
            val x16 = x15 * 2
            x16
          }
          val x18 = x4
          val x19 = x17(x18)
          val x20 = x17(x1)
          val x21 = x19 + x20
          val x22 = x3
          val x23 = x17(x22)
          val x24 = x21 + x23
          x4 = x24
          val x26 = x4
          x26
        }
        val x28 = x5()
        x28
      } else {
        val x30 = x4
        exit(x30)
      }
      x32
    }
    val x33 = x5()
    x33
  }
}
// output:
