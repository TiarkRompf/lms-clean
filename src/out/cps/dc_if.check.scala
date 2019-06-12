class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    val x11 = x3 > 5
    def cIf0(x16: Int) = {
      val x18 = {
        def x6(x10: Int) = {
          val x17 = x10 * x16
          x17
        }
        val x7 = x6(x1)
        val x8 = x6(x7)
        val x9 = x8 + 3
        x9
      }
      val x19 = x18 + 10
      x19 /*exit x19*/
    }
    if (x11) {
      val x13 = x1 - 5
      cIf0(x13)
    } else {
      val x15 = x1 + 5
      cIf0(x15)
    }
  }
}
// output:
