class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    var x3 = 0
    var x4 = 0
    while ({
      val x6 = x3
      val x7 = x6 < 5
      x7
    }) {
      val x9 = x3
      val x10 = x9 + 1
      x3 = x10
      val x26 = {
        def x16(x14: Int): Int = {
          val x15 = x14 * 2
          x15
        }
        val x17 = x4
        val x18 = x16(x17)
        val x19 = x16(x1)
        val x20 = x18 + x19
        val x21 = x3
        val x22 = x16(x21)
        val x23 = x20 + x22
        x4 = x23
        val x25 = x4
        x25
      }
      ()
    }
    val x28 = x4
    exit(x28)
  }
}
// output:
