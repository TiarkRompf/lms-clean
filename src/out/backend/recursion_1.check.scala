/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Int) {
  def apply(x0: Int): Int = {
    var x1: Int = null.asInstanceOf[Int]
    var x2: scala.Function1[Int, Int] = null.asInstanceOf[scala.Function1[Int, Int]]
    x1 = 3
    x2 = x3
    def x3(x4:Int): Int = {
      if (x4 > 0) {
        x4 * (x2(x4 - 1))
      } else {
        1
      }
    }
    x3(x1)
  }
}
/*****************************************
End of Generated Code
*******************************************/
// output:
