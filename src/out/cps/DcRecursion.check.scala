class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x21 = {
      def x4(x18: Int) = {
        val x19 = x18 * 2
        val x20 = x19 + 3
        x20
      }
      lazy val x5 = x6 _
      def x6(c: Int => Int, x8: Int): Int = {
        val x9 = x8 > 0
        def cIf0(x16: Int) = {
          c(x16)
        }
        if (x9) {
          val x11 = x4(x8)
          val x12 = x8 - 1
          def cApp1(x13: Int) = {
            val x14 = x11 * x13
            cIf0(x14)
          }
          x5(cApp1, x12)
        } else {
          val x11 = x4(x8)
          cIf0(x11)
        }
      }
      def cApp2(x17: Int) = {
        x17
      }
      x6(cApp2, x1)
    }
    val x22 = x21 + 10
    x22 /*exit x22*/
  }
}
// output:
