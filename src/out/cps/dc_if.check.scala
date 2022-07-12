class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    val x12 = x3 > 5
    def cIf0(x17: Int) = {
      val x19 = {
        def x6(x10: Int) = {
          val x18 = x10 * x17
          x18
        }
        val x7 = x6(x1)
        val x8 = x6(x7)
        val x9 = x8 + 3
        x9
      }
      val x20 = x19 + 10
      x20 /*exit x20*/
    }
    if (x12) {
      val x14 = x1 - 5
      cIf0(x14)
    } else {
      val x16 = x1 + 5
      cIf0(x16)
    }
  }
}
// output:
