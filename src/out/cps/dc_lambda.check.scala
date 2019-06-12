class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x3 = x1 * 2
    def x6(c: Int => Int, x8: Int): Int = {
      val x9 = x8 * 2
      c(x9)
    }
    def cApp0(x15: Int) = {
      def cApp1(x16: Int) = {
        val x18 = {
          def x11(x14: Int) = {
            val x17 = x14 + x16
            x17
          }
          val x12 = x11(x1)
          val x13 = x11(x12)
          x13
        }
        val x19 = x18 + 5
        x19 /*exit x19*/
      }
      x6(cApp1, x15)
    }
    x6(cApp0, x3)
  }
}
// output:
