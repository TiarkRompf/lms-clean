/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 100
    val x2 = ({ (x3: Int) =>
      x1 = 50
      new Array[Int](x3)
    })(2)
    x2(0) = 1
    println(x2(1))
    println(x1)
  }
}
/*****************************************
End of Generated Code
*******************************************/
