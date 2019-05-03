class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 > 5
    def cIf0(x16: Int) = {
      x16 /*exit x16*/
    }
    if (x2) {
      val x12 = {
        def x6(x10: Int) = {
          val x11 = x10 * 2
          x11
        }
        val x7 = x6(x1)
        val x8 = x6(x7)
        val x9 = x8 + 3
        x9
      }
      val x13 = x12 + 10
      cIf0(x13)
    } else {
      val x15 = x1 + 5
      cIf0(x15)
    }
  }
}
// output:
