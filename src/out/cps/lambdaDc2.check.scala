class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x3(c: Int => Int, x5: Int): Int = {
      val x14 = {
        def x8(x12: Int) = {
          val x13 = x12 * 2
          x13
        }
        val x9 = x8(x5)
        val x10 = x8(x1)
        val x11 = x9 + x10
        x11
      }
      val x15 = x14 + 5
      c(x15)
    }
    val x16 = x1 + 2
    def cApp0(x17: Int) = {
      exit(x17)
    }
    x3(cApp0, x16)
  }
}
// output:
