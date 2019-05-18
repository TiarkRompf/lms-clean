/*****************************************
Emitting Generated Code
*******************************************/
class Snippet() extends (java.lang.String => Unit) {
  def apply(x0: java.lang.String): Unit = {
    x0 match {
      case "hello" =>
      println("hello")
      case "A" | "B" | "C" =>
      println("Hello")
      println(x0)
    }
  }
}
/*****************************************
End of Generated Code
*******************************************/
// Output:
compilation: ok
Hello
A
Hello
C
hello
