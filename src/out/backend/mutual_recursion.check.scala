/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1: Function1[Int, Boolean] = null.asInstanceOf[Function1[Int, Boolean]]
    x1 = x2
    def x4(x3:Int): Boolean = x3 == 1 || x1(x3 - 1)
    def x2(x5:Int): Boolean = x5 == 0 || x4(x5 - 1)
    printf("%d", x2(x0))
  }
}
/*****************************************
End of Generated Code
*******************************************/
