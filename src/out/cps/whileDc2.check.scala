class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 0
    var x3 = 0
    def loop0(): Int = {
      val x5 = x2
      val x6 = x5 < 5
      if (x6) {
        val x8 = x2
        val x9 = x8 + 1
        x2 = x9
        val x25 = {
          def x13(x23: Int) = {
            val x24 = x23 * 2
            x24
          }
          val x14 = x3
          val x15 = x13(x14)
          val x16 = x13(x1)
          val x17 = x15 + x16
          val x18 = x2
          val x19 = x13(x18)
          val x20 = x17 + x19
          x3 = x20
          x20
        }
        loop0()
      } else {
        val x28 = x3
        x28 /*exit x28*/
      }
    }
    loop0()
  }
}
// output:
