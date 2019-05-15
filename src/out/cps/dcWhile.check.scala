class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x26 = {
      def x4(x24: Int) = {
        val x25 = x24 * 2
        x25
      }
      var x5 = 0
      var x6 = 0
      def loop0(): Int = {
        val x8 = x5
        val x9 = x8 < 5
        if (x9) {
          val x11 = x6
          val x12 = x4(x11)
          val x13 = x4(x1)
          val x14 = x12 + x13
          val x15 = x5
          val x16 = x4(x15)
          val x17 = x14 + x16
          x6 = x17
          val x19 = x5
          val x20 = x19 + 1
          x5 = x20
          loop0()
        } else {
          val x23 = x6
          x23
        }
      }
      loop0()
    }
    val x27 = x26 + 4
    x27 /*exit x27*/
  }
}
// output:
