class Snippet extends (Int => Int) {
  def apply(x1: Int): Int = {
    def exit(res: Int): Int = res
    def x3(c: Int => Int, x5: Int): Int = {
      val x6 = x5 * 2
      c(x6)
    }
    def cApp0(x7: Int) = {
      def cApp1(x8: Int) = {
        exit(x8)
      }
      x3(cApp1, x7)
    }
    x3(cApp0, x1)
  }
}
// output:
