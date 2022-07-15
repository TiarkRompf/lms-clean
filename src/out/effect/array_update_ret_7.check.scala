/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Tuple2[Array[Int], Int] => Tuple2[Array[Int], Int]) {
  def apply(x0: Tuple2[Array[Int], Int]): Tuple2[Array[Int], Int] = {
    val x1 = x0._1
    x1(0) = 0
    (x1, x0._2 + 1)
  }
}
/*****************************************
End of Generated Code
*******************************************/
