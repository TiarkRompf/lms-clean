class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x17(x3: Int => Int, x5: Int): Int = {
      val x14 = {
        def x10(x8: Int): Int = {
          val x9 = x8 * 2
          x9
        }
        val x11 = x10(x5)
        val x12 = x10(x1)
        val x13 = x11 + x12
        x13
      }
      val x15 = x14 + 5
      val x16 = x3(x15)
      x16
    }
    val x18 = x1 + 2
    def x22(x20: Int): Int = {
      exit(x20)
    }
    val x23 = x17(x22, x18)
    x23
  }
}
// output:
