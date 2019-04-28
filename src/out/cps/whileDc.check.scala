class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x27 = {
      var x3 = 0
      var x4 = 0
      def loop0(): Int = {
        val x6 = x3
        val x7 = x6 < 5
        if (x7) {
          def x10(x20: Int) = {
            val x22 = x3
            val x23 = x22 + 1
            x3 = x23
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
          val x19 = x4
          x19
        } else {
          val x26 = x4
          x26
        }
      }
      loop0()
    }
    val x28 = x27 + 4
    exit(x28)
  }
}
// output:
