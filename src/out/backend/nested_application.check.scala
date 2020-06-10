/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 0
    var x2 = 1
    val x3 = ({ (x4: Int) =>
      x1 = 1
      x4 + 1
    })(({ (x5: Int) =>
      x2 = 3
      x5 + 2
    })(x0))
    printf("%d, %d, %d", x1, x2, x3)
  }
}
/*****************************************
End of Generated Code
*******************************************/
