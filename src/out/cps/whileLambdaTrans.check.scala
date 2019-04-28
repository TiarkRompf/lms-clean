class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x8(x3: Int => Int, x5: Int): Int = {
      val x6 = 2 * x5
      val x7 = x3(x6)
      x7
    }
    var x9 = 0
    var x10 = 0
    def x11(): Int = {
      val x13 = x9
      val x14 = x13 < x1
      val x31 = if (x14) {
        val x16 = x10
        val x17 = x9
        def x26(x19: Int): Int = {
          val x20 = x16 + x19
          x10 = x20
          val x22 = x9
          val x23 = x22 + 1
          x9 = x23
          val x25 = x11()
          x25
        }
        val x27 = x8(x26, x17)
        x27
      } else {
        val x29 = x10
        exit(x29)
      }
      x31
    }
    val x32 = x11()
    x32
  }
}
// output:
