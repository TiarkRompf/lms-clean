/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1: Function1[Int, Int] = null.asInstanceOf[Function1[Int, Int]]
    def x4(x2: Int, x3: Int): Int = x2 + x3
    x1 = x5
    def x5(x6: Int): Int = if (x6 == 1) 1 else {
      val x7 = x6 / 2
      x4(x1(x7),x1(x7))
    }
    printf("%d %d", x5(x0), x5(x0))
  }
}
/*****************************************
End of Generated Code
*******************************************/
