class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def x3(c: Int => Unit, x5: Int): Unit = {
      val x6 = 2 * x5
      c(x6)
    }
    var x7 = 0
    var x8 = 0
    def loop0(): Unit = {
      val x10 = x7
      val x11 = x10 < x1
      if (x11) {
        val x13 = x8
        val x14 = x7
        def cApp1(x15: Int) {
          val x16 = x13 + x15
          x8 = x16
          val x18 = x7
          val x19 = x18 + 1
          x7 = x19
          loop0()
        }
        x3(cApp1, x14)
      } else {
        val x22 = x8
        assert(x22 == 5, "wants 5, gets " + x22)
      }
    }
    loop0()
    1
  }
}
// output:
