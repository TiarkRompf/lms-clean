/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    val x1 = (if (x0 == 0) {
      var x2 = 100
      { (x3: Int) =>
        val x4 = new Array[Int](x3)
        x4(0) = x2
        x4
      }
    } else {
      var x5 = 40
      { (x6: Int) =>
        val x7 = new Array[Int](x6)
        x7(0) = x5
        x7
      }
    })(3)
    x1(1) = 70
    println(x1(0))
    println(70)
    println(x1(2))
  }
}
/*****************************************
End of Generated Code
*******************************************/
