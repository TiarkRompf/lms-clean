class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x30 = {
      def x7(x5: Int): Int = {
        val x6 = x5 + 4
        x6
      }
      val x8 = x7(x1)
      val x9 = 2 * x8
      val x28 = {
        def x14(x12: Int): Int = {
          val x13 = x12 + 10
          x13
        }
        val x15 = x14(x1)
        val x16 = x14(x15)
        val x26 = {
          def x22(x19: Int): Int = {
            val x20 = x19 * 2
            val x21 = 11 + x20
            x21
          }
          val x23 = x22(x1)
          val x24 = x22(14)
          val x25 = x23 + x24
          x25
        }
        val x27 = x16 + x26
        x27
      }
      val x29 = x9 * x28
      x29
    }
    val x31 = x30 * x1
    exit(x31)
  }
}
// output:
