class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x22 = {
      def x4(x19: Int) = {
        val x20 = x19 * 2
        val x21 = x20 + 3
        x21
      }
      lazy val x5 = x6 _
      def x6(c: Int => Int, x8: Int): Int = {
        val x9 = x8 > 0
        def cIf0(x17: Int) = {
          c(x17)
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
          val x16 = x4(x8)
          cIf0(x16)
        }
      }
      def cApp2(x18: Int) = {
        x18
      }
      x6(cApp2, x1)
    }
    val x23 = x22 + 10
    x23 /*exit x23*/
  }
}
// output:
