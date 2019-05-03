class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x15 = {
      def x4(x7: Int) = {
        def x9(x12: Int) = {
          val x13 = x7 + x12
          val x14 = x13 + 4
          x14
        }
        val x10 = x9(x1)
        val x11 = 3 * x10
        x11
      }
      val x5 = x4(x1)
      val x6 = 2 * x5
      x6
    }
    val x16 = x15 * x1
    x16 /*exit x16*/
  }
}
// output:
