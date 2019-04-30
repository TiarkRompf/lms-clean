class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x3 = x1 > 5
    def x7(x5: Int): Int = {
      exit(x5)
    }
    val x23 = if (x3) {
      val x17 = {
        def x13(x11: Int): Int = {
          val x12 = x11 * 2
          x12
        }
        val x14 = x13(x1)
        val x15 = x13(x14)
        val x16 = x15 + 3
        x16
      }
      val x18 = x17 + 10
      val x19 = x7(x18)
      x19
    } else {
      val x21 = x1 + 5
      val x22 = x7(x21)
      x22
    }
    x23
  }
}
// output:
