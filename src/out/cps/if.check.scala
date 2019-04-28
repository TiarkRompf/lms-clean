class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x2 = x1 + x1
    val x3 = x2 > 2
    def cIf0(x10: Int) = {
      exit(x10)
    }
    if (x3) {
      val x5 = x2 * x1
      val x6 = x5 + x1
      cIf0(x6)
    } else {
      val x8 = x2 / x1
      val x9 = x8 - x1
      cIf0(x9)
    }
  }
}
// output:
