/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    val x1 = new Array[Int](10)
    (if (x0 == 0) { (x2: Array[Int]) =>
      x2(0) = 8
      1
    } else { (x3: Array[Int]) =>
      x3(0) = 9
      2
    })(x1)
    printf("%d %d", x1(0), x1(1))
  }
}
/*****************************************
End of Generated Code
*******************************************/
