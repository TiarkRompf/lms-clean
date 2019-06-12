/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Int) {
  def apply(x0: Int): Int = {
    var x1: scala.Function1[Int, Int] = null.asInstanceOf[scala.Function1[Int, Int]]
    x1 = x2
    def x2(x3:Int): Int = if (x3 > 0) x3 * x1(x3 - 1) else 1
    x2(3)
  }
}
/*****************************************
End of Generated Code
*******************************************/
// output:
