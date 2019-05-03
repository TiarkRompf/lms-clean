class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x10 = {
      val x3 = println("A")
      def x5(x8: Int) = {
        val x9 = println("B")
        1
      }
      val x6 = x5(x1)
      val x7 = x5(x6)
      x7
    }
    val x11 = x10 + 5
    x11 /*exit x11*/
  }
}
// output:
