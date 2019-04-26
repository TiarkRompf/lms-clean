class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    lazy val x8 = x9 _
    def x9(c: Int => Unit, x11: Int): Unit = {
      val x12 = x11 > 0
      def cIf0(x18: Int) = {
        c(x18)
      }
      if (x12) {
        val x14 = x11 - x1
        def cApp1(x15: Int) {
          val x16 = x11 * x15
          cIf0(x16)
        }
        x8(cApp1, x14)
      } else {
        cIf0(1)
      }
    }
    def loop2(): Unit = {
      val x5 = x2
      val x6 = x5 < 10
      if (x6) {
        val x19 = x2
        def cApp3(x20: Int) {
          x3 = x20
          val x22 = x2
          val x23 = x22 + 1
          x2 = x23
          loop2()
        }
        x9(cApp3, x19)
      } else {
        val x26 = x3
        assert(x26 == 5, "wants 5, gets " + x26)
      }
    }
    loop2()
    1
  }
}
// output:
