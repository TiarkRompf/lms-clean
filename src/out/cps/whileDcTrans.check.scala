class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x31 = {
      var x4 = 0
      var x5 = 0
      def x6(): Int = {
        val x8 = x4
        val x9 = x8 < 5
        val x29 = if (x9) {
          def x17(x12: Int): Int = {
            val x13 = x4
            val x14 = x13 + 1
            x4 = x14
            val x16 = x6()
            x16
          }
          val x18 = x5
          val x19 = x17(x18)
          val x20 = x17(x1)
          val x21 = x19 + x20
          val x22 = x4
          val x23 = x17(x22)
          val x24 = x21 + x23
          x5 = x24
          val x26 = x5
          x26
        } else {
          val x28 = x5
          x28
        }
        x29
      }
      val x30 = x6()
      x30
    }
    val x32 = x31 + 4
    exit(x32)
  }
}
// output:
