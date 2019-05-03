class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x7(x2: Int => Int, x4: Int): Int = {
      val x5 = 2 * x4
      val x6 = x2(x5)
      x6
    }
    var x8 = 0
    var x9 = 0
    def x10(): Int = {
      val x12 = x8
      val x13 = x12 < x1
      val x30 = if (x13) {
        val x15 = x9
        val x16 = x8
        def x25(x18: Int): Int = {
          val x19 = x15 + x18
          x9 = x19
          val x21 = x8
          val x22 = x21 + 1
          x8 = x22
          val x24 = x10()
          x24
        }
        val x26 = x7(x25, x16)
        x26
      } else {
        val x28 = x9
        x28 /*exit: x28 */
      }
      x30
    }
    val x31 = x10()
    x31
  }
}
// output:
