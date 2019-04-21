package lms.core

import java.io._

object utils {
  val profilerMap = new scala.collection.mutable.HashMap[String, PrintStream]()
  def time[A](key: String, saveToFile: Boolean = false, append: Boolean = false)(a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    if (saveToFile) {
      val file = profilerMap.getOrElseUpdate(key, new PrintStream(new FileOutputStream(new File(s"$key.timing"), append)))
      file.println(micros)
      file.flush
    }
    println(s"$key: $micros µs")
    result
  }
  def captureOut(func: => Any): String = {
    val source = new java.io.ByteArrayOutputStream()
    withOutput(new java.io.PrintStream(source))(func)
    source.toString
  }
  def withOutput[T](out: PrintStream)(f: => Unit): Unit = {
    scala.Console.withOut(out)(scala.Console.withErr(out)(f))
  }
  def devnull(f: => Unit): Unit = {
    withOutput(nullout)(f)
  }
  def nullout = new PrintStream(new OutputStream() {
    override def write(b: Int) = {}
    override def write(b: Array[Byte]) = {}
    override def write(b: Array[Byte], off: Int, len: Int) = {}
  })
  def withOutputFull(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      scala.Console.withOut(out)(scala.Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }
}
