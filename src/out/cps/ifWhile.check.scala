class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x2 = x1 + x1
    val x5 = x2 > 10
    def cIf0(x36: Int) = {
      x36 /*exit x36*/
    }
    if (x5) {
      var x3 = 0
      var x4 = 0
      val x9 = x2 - 10
      def loop1(): Int = {
        val x8 = x3
        val x10 = x8 < x9
        if (x10) {
          val x12 = x4
          val x13 = x3
          val x14 = x12 + x13
          x4 = x14
          val x16 = x3
          val x17 = x16 + 1
          x3 = x17
          loop1()
        } else {
          val x20 = x4
          cIf0(x20)
        }
      }
      loop1()
    } else {
      var x3 = 0
      var x4 = 0
      val x24 = x2 + 10
      def loop2(): Int = {
        val x23 = x3
        val x25 = x23 < x24
        if (x25) {
          val x27 = x4
          val x28 = x3
          val x29 = x27 + x28
          x4 = x29
          val x31 = x3
          val x32 = x31 + 1
          x3 = x32
          loop2()
        } else {
          val x35 = x4
          cIf0(x35)
        }
      }
      loop2()
    }
  }
}
// output:
