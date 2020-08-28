/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    val x1 = ((x2: Int) => x2 + 10)(x0)
    val x3 = ((x4: Int) => x4 * 2)(x0)
    printf("%d", if (x0 > 10) x1 - x3 else x1 + x3)
  }
}
/*****************************************
End of Generated Code
*******************************************/
