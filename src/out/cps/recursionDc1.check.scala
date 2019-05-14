class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x20 = {
      lazy val x3 = x4 _
      def x4(c: Int => Int, x6: Int): Int = {
        val x7 = x6 > 0
        def cIf0(x18: Int) = {
          c(x18)
        }
        if (x7) {
          def x10(x13: Int) = {
            val x14 = x6 - 1
            def cApp1(x15: Int) = {
              val x16 = x13 + x15
              cIf0(x16)
            }
            x3(cApp1, x14)
          }
          val x11 = x10(x6)
          val x12 = x10(x11)
          x12
        } else {
          cIf0(1)
        }
      }
      def cApp2(x19: Int) = {
        x19
      }
      x4(cApp2, x1)
    }
    x20 /*exit x20*/
  }
}
// output:
