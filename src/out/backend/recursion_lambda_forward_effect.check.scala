/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1: Array[Int] = null.asInstanceOf[Array[Int]]
    var x2: scala.Function2[Int, Array[Int], Unit] = null.asInstanceOf[scala.Function2[Int, Array[Int], Unit]]
    x1 = Array(1, 1, 1, 1, 1)
    x2 = x3
    def x3(x4: Int, x5: Array[Int]): Unit = {
      printf("%d\n", x5(x4))
      if (x4 > 0) {
        val x6 = Array(x4, x4, x4, x4, x4)
        val x7 = x4 - 1
        x6(x7) = 100
        x2(x7,x6)
      }
    }
    x3(3,x1)
  }
}
/*****************************************
End of Generated Code
*******************************************/
// output:
compilation: ok
1
100
100
100
