class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x19 = {
      val x3 = x1 * 2
      var x4 = x3
      def x6(c: Int => Int, x8: Int): Int = {
        val x9 = x8 * 2
        c(x9)
      }
      def x11(x14: Int) = {
        val x15 = x4
        def cApp0(x16: Int) = {
          def cApp1(x17: Int) = {
            val x18 = x14 + x17
            x18
          }
          x6(cApp1, x16)
        }
        x6(cApp0, x15)
      }
      val x12 = x11(x1)
      val x13 = x11(x12)
      x13
    }
    val x20 = x19 + 5
    x20 /*exit x20*/
  }
}
// output:
