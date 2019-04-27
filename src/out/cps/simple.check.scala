class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = return res
    val x2 = x1 + x1
    val x3 = x2 * x1
    val x4 = x3 / x3
    exit(x4)
  }
}
// output:
