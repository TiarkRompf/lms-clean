/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 0
    while (x1 < 10) {
      var x2: Int = null.asInstanceOf[Int]
      var x3: scala.Function1[Int, Int] = null.asInstanceOf[scala.Function1[Int, Int]]
      x2 = x1
      x3 = x4
      def x4(x5:Int): Int = if (x5 > 0) x2 * x3(x5 - x0) else 1
      x4(x1)
      x1 = x1 + 1
    }
  }
}
/*****************************************
End of Generated Code
*******************************************/
// output:
