class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x2 = x1 + x1
    var x3 = x2
    var x4 = 0
    def loop0(): Int = {
      val x6 = x3
      val x7 = x6 > 0
      if (x7) {
        val x9 = x4
        val x10 = x3
        val x11 = x9 + x10
        x4 = x11
        val x13 = x3
        val x14 = x13 - 1
        x3 = x14
        loop0()
      } else {
        val x17 = x4
        exit(x17)
      }
    }
    loop0()
  }
}
// output:
