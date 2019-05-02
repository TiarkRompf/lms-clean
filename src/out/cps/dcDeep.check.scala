class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    val x29 = {
      def x4(x27: Int) = {
        val x28 = x27 + 4
        x28
      }
      val x5 = x4(x1)
      val x6 = 2 * x5
      val x25 = {
        def x9(x23: Int) = {
          val x24 = x23 + 10
          x24
        }
        val x10 = x9(x1)
        val x11 = x9(x10)
        val x21 = {
          def x14(x18: Int) = {
            val x19 = x18 * 2
            val x20 = 11 + x19
            x20
          }
          val x15 = x14(x1)
          val x16 = x14(14)
          val x17 = x15 + x16
          x17
        }
        val x22 = x11 + x21
        x22
      }
      val x26 = x6 * x25
      x26
    }
    val x30 = x29 * x1
    exit(x30)
  }
}
// output:
