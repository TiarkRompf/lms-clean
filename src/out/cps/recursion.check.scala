class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x3(c: Int => Int, x5: Int): Int = {
      val x6 = x5 > 0
      def cIf0(x12: Int) = {
        c(x12)
      }
      if (x6) {
        val x8 = x5 - 1
        def cApp1(x9: Int) = {
          val x10 = x5 * x9
          cIf0(x10)
        }
        x2(cApp1, x8)
      } else {
        cIf0(1)
      }
    }
    def cApp2(x13: Int) = {
      x13 /*exit x13*/
    }
    x3(cApp2, x1)
  }
}
// output:
