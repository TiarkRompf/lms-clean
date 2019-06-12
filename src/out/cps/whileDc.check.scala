class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x26 = {
      var x3 = 0
      var x4 = 0
      def loop0(): Int = {
        val x6 = x3
        val x7 = x6 < 5
        if (x7) {
          def x10(x19: Int) = {
            val x21 = x3
            val x22 = x21 + 1
            x3 = x22
            loop0()
          }
          val x11 = x4
          val x12 = x10(x11)
          val x13 = x10(x1)
          val x14 = x12 + x13
          val x15 = x3
          val x16 = x10(x15)
          val x17 = x14 + x16
          x4 = x17
          x17
        } else {
          val x25 = x4
          x25
        }
      }
      loop0()
    }
    val x27 = x26 + 4
    x27 /*exit x27*/
  }
}
// output:
