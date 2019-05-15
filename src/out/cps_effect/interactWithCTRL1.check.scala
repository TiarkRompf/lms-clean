class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    val x10 = {
      val x3 = println("A")
      val x4 = println("B")
      def x6(x9: Int) = {
        1
      }
      val x7 = x6(x1)
      val x8 = x6(x7)
      x8
    }
    val x11 = x10 + 5
    x11 /*exit x11*/
  }
}
// output:
