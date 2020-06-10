/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 0
    var x2 = 1
    printf("%d, %d, %d", ({ (x3: Int) =>
      x1 = 1
      { (x4: Int) =>
        x2 = 2
        x4 + x3
      }
    })(x0)(x0), x1, x2)
  }
}
/*****************************************
End of Generated Code
*******************************************/
