/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    val x1 = new Array[Int](1)
    ({ (x2: Array[Int]) =>
      x2(0) = 5
    })(x1)
    println(x1(0))
  }
}
/*****************************************
End of Generated Code
*******************************************/
