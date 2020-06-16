/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 100
    val x2 = ({ (x3: Int) =>
      val x4 = new Array[Int](x3)
      x4(0) = x1
      x1 = 20
      x4
    })(3)
    val x5 = x1
    x2(1) = x5
    println(x2(0))
    println(x5)
    println(x2(2))
  }
}
/*****************************************
End of Generated Code
*******************************************/
