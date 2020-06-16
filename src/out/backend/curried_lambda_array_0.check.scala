/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    val x1 = new Array[Int](10)
    val x2 = new Array[Int](5)
    def x4(x3:Array[Int]): Unit = {
      x3(2) = 3
    }
    def x6(x5:Array[Int]): Function1[Array[Int], Unit] = {
      x5(1) = 2
      x4
    }
    ({ (x7: Array[Int]) =>
      x7(0) = 1
      x6
    })(x1)(x1)(x2)
    printf("%d, %d, %d, %d", x1(0), x1(1), x2(0), x2(2))
  }
}
/*****************************************
End of Generated Code
*******************************************/
