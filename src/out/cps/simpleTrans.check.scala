class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = return res
    val x3 = x1 + x1
    val x4 = x3 * x1
    val x5 = x4 / x4
    exit(x5)
  }
}
// output:
