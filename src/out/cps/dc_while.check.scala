class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    val x25 = {
      def loop0(): Int = {
        val x6 = x2
        val x7 = x6 < 5
        if (x7) {
          val x9 = x3
          val x10 = x2
          val x11 = x9 + x10
          x3 = x11
          val x13 = x2
          val x14 = x13 + 1
          x2 = x14
          loop0()
        } else {
          val x17 = x3
          def x19(x22: Int) = {
            val x23 = x17 * x22
            val x24 = x23 * 2
            x24
          }
          val x20 = x19(x1)
          val x21 = x19(x20)
          x21
        }
      }
      loop0()
    }
    val x26 = x25 + 4
    x26 /*exit x26*/
  }
}
// output:
