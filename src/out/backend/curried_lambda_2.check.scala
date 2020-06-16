/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 0
    var x2 = 1
    var x3 = 2
    printf("%d %d %d %d", ({ (x4: Int) =>
      x1 = 1
      { (x5: Int) =>
        x2 = 2
        val x6 = x4 + x5
        { (x7: Int) =>
          x3 = 3
          x6 + x7
        }
      }
    })(x0)(x0)(x0), x1, x2, x3)
  }
}
/*****************************************
End of Generated Code
*******************************************/
