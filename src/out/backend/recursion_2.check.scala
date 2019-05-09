/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1: Int = null.asInstanceOf[Int]
    var x2: scala.Function1[Int, Int] = null.asInstanceOf[scala.Function1[Int, Int]]
    x1 = 0
    x2 = x3
    def x3(x4:Int): Int = if (x4 > 0) x4 * x2(x4 - x0) else 1
    while (x1 < 10) {
      println(x3(x1))
      x1 = x1 + 1
    }
  }
}
/*****************************************
End of Generated Code
*******************************************/
// output:
1
1
2
6
24
120
720
5040
40320
362880
1
1
2
3
8
15
48
105
384
945
