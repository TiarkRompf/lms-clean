class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    var x2 = 3
    var x3 = 2
    val x11 = {
      val x5 = x2
      val x6 = x5 + 2
      x2 = x6
      val x8 = x3
      val x9 = x8 + 3
      x3 = x9
      ()
    }
    val x12 = x2
    x12 /*exit: x12 */
  }
}
// output:
