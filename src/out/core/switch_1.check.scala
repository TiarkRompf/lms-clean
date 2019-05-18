/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (Int => Unit) {
  def apply(x0: Int): Unit = {
    var x1 = 0
    x0 match {
      case 0 =>
      printf("zero %d\n", 0)
      case 1 | 2 | 3 =>
      x1 = x0
      case _ =>
      printf("%d\n", x0)
    }
    printf("Final value %d\n", x1)
  }
}
/*****************************************
End of Generated Code
*******************************************/
// Output:
zero 0
Final value 0
Final value 2
5
Final value 0
