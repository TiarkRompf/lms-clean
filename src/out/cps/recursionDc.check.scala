class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x3(c: Int => Int, x5: Int): Int = {
      val x20 = {
        def x8(x17: Int) = {
          val x18 = x17 * 2
          val x19 = x18 + 3
          x19
        }
        val x9 = x5 > 0
        def cIf0(x16: Int) = {
          x16
        }
        if (x9) {
          val x11 = x8(x5)
          val x12 = x5 - 1
          def cApp1(x13: Int) = {
            val x14 = x11 * x13
            cIf0(x14)
          }
          x2(cApp1, x12)
        } else {
          val x11 = x8(x5)
          cIf0(x11)
        }
      }
      val x21 = x20 + 10
      c(x21)
    }
    def cApp2(x22: Int) = {
      x22 /*exit x22*/
    }
    x3(cApp2, x1)
  }
}
// output:
