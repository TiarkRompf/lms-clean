package lms.core

import java.io._

object utils {
  lazy val defaultProfileStream = new PrintStream(new FileOutputStream(new File(s"timing.log")))
  lazy val defaultProfileInvStream = new PrintStream(new FileOutputStream(new File(s"timingInv.log")))
  var customProfileStream: Option[PrintStream] = None
  var customProfileInvStream: Option[PrintStream] = None
  def profileStream = customProfileStream.getOrElse(defaultProfileStream)
  def profileInvStream = customProfileInvStream.getOrElse(defaultProfileInvStream)
  var scope = List[String]()
  var timeInSub = 0L

  def time[A](key: String)(a: => A) = {
    val start = System.nanoTime
    val saveTime = timeInSub
    scope = key::scope
    timeInSub = 0
    try a finally {
      val timeElapsed = (System.nanoTime - start) / 1000
      val timeHere = timeElapsed - timeInSub

      profileStream.println(s"${scope.reverse.mkString(";")} $timeHere")
      profileInvStream.println(s"${scope.mkString(";")} $timeHere")

      scope = scope.tail;
      timeInSub = saveTime
      timeInSub += timeElapsed // account for time spent here in parent
    }
  }
  def withTiming[T](profileStream: PrintStream, profileInvStream: PrintStream)(f: => T): T = {
    val prevProfileStream = customProfileStream
    val prevProfileInvStream = customProfileInvStream

    customProfileStream = Some(profileStream)
    customProfileInvStream = Some(profileInvStream)

    val res = f

    customProfileStream = prevProfileStream
    customProfileInvStream = prevProfileInvStream

    res
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
