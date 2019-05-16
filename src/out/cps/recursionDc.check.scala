class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    lazy val x2 = x3 _
    def x3(c: Int => Int, x5: Int): Int = {
      val x9 = x5 > 0
      val x12 = x5 - 1
      val x21 = {
        def x8(x18: Int) = {
          val x19 = x18 * 2
          val x20 = x19 + 3
          x20
        }
        def cIf0(x17: Int) = {
          x17
        }
        if (x9) {
          val x11 = x8(x5)
          def cApp1(x13: Int) = {
            val x14 = x11 * x13
            cIf0(x14)
          }
          x2(cApp1, x12)
        } else {
          val x16 = x8(x5)
          cIf0(x16)
        }
      }
      val x22 = x21 + 10
      c(x22)
    }
    def cApp2(x23: Int) = {
      x23 /*exit x23*/
    }
    x3(cApp2, x1)
  }
}
// output:
