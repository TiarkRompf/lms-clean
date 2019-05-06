package lms
package demos
package fft

import lms.util.GraphUtil
import lms.core._
import Backend._


trait FFT extends FrontEnd {

  // boilerplate, should go in core
  case class DOUBLE(x: Exp) {
    def +(y: DOUBLE): DOUBLE = DOUBLE(g.reflect("+",x,y.x))
    def -(y: DOUBLE): DOUBLE = DOUBLE(g.reflect("-",x,y.x))
    def *(y: DOUBLE): DOUBLE = DOUBLE(g.reflect("*",x,y.x))
    def /(y: DOUBLE): DOUBLE = DOUBLE(g.reflect("/",x,y.x))
  }

  def sin(x: DOUBLE): DOUBLE = DOUBLE(g.reflect("sin", x.x))
  def cos(x: DOUBLE): DOUBLE = DOUBLE(g.reflect("cos", x.x))

  case class DARRAY(x: Exp) {
    def apply(i: INT): DOUBLE = DOUBLE(g.reflectRead("array_get",x,i.x)(x))
    def update(i: INT, y: DOUBLE): Unit = g.reflectWrite("array_set",x,i.x,y.x)(x)
  }
  object DARRAY {
    def apply(n: INT): DARRAY = DARRAY(g.reflectMutable("array_new",n.x))
  }

  implicit def liftDouble(x: Double): DOUBLE = DOUBLE(Const(x))


  // fft algo

  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))
  }

  case class Complex(re: DOUBLE, im: DOUBLE) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)
    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)
    def *(that: Complex) = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
  }

  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((e :: es), (o :: os)) =>
      e :: (o :: mergeEvenOdd(es, os))
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

  def fft(n: Int)(xs: DARRAY): Unit = {
    val in = List.tabulate(n)(i => Complex(xs(2*i), xs(2*i+1)))
    val out = fft(in)
    for (i <- 0 until n) {
      xs(2*i)   = out(i).re
      xs(2*i+1) = out(i).im
    }
  }

  override def mkGraphBuilder() = new MyGraphBuilder()

  class MyGraphBuilder extends GraphBuilder {
    //rewrites = FFT.this.rewrites

    override def reflect(s: String, as: Def*): Exp = (s,as.toList) match {
      // case ("sin", List(Const(a:Double))) => Const(math.sin(a))
      // case ("cos", List(Const(a:Double))) => Const(math.cos(a))
      case ("+", List(Const(a:Double),Const(b:Double))) => Const(a+b)
      case ("+", List(Const(a:Double),Const(b:Double))) => Const(a+b)
      case ("-", List(Const(a:Double),Const(b:Double))) => Const(a-b)
      case ("*", List(Const(a:Double),Const(b:Double))) => Const(a*b)
      case ("/", List(Const(a:Double),Const(b:Double))) => Const(a/b)
      case ("%", List(Const(a:Double),Const(b:Double))) => Const(a%b)

      case p =>
        super.reflect(s, as:_*)
    }
  }
}




/*
trait ArithExpOptFFT extends ArithExpOpt {

  override def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => infix_-(x, y)
    case _ => super.infix_+(x, y)
  }

  override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => infix_+(x, y)
    case _ => super.infix_-(x, y)
  }

  override def infix_*(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
    case (x, Const(-1.0)) => infix_-(0.0, x)
    case (Const(-1.0), y) => infix_-(0.0, y)
    case _ => super.infix_*(x, y)
  }
}



trait TrigExpOptFFT extends TrigExpOpt {
  override def cos(x: Exp[Double]) = x match {
    case Const(x) if { val z = x / math.Pi / 0.5; z != 0 && z == z.toInt } => Const(0.0)
    case _ => super.cos(x)
  }
}
*/


class FFTTest extends TutorialFunSuite {
  val under = "demos/fft/fft-"

  val fe = new FFT {}
  import fe._

  val sc = new util.ScalaCompile {}
  sc.dumpGeneratedCode = true

  def mkClassName(name: String) = {
    // mangle class name
    ("fft-" + name).replace("-","_")
  }

  def testBE(name: String, verbose: Boolean = false, alt: Boolean = false, eff: Boolean = false)(prog: DARRAY => Unit) = {
    test(name) {
      checkOut(name, "scala", {
        var g = program({ x => prog(DARRAY(x.x)); INT(Const(())) }) //XXX hack!
        g = HardenMayHardDeps(g)

        if (verbose) {
          println("// Raw:")
          g.nodes.foreach(println)

          println("// Generic Codegen:")
          (new GenericCodeGen)(g)

          println("// Scala Codegen:")
          (new ScalaCodeGen)(g)

          println("// Compact Scala Codegen:")
          (new CompactScalaCodeGen)(g)
        }

        def emitSource() = {
          val cg = new CompactScalaCodeGen
          if (!verbose) cg.doRename = true
          if (eff)      cg.doPrintEffects = true

          val arg = cg.quote(g.block.in.head)
          val efs = cg.quoteEff(g.block.in.last)
          var src = utils.captureOut(cg(g))

          if (!verbose) {
            // remove unused val x1 = ...
            val names = cg.rename.map(p => p._2).toSet
            for (n <- names) {
              val removed = src.replace(s"val $n = ","")
              if (removed.indexOf(n) < 0)
                src = removed
            }
          }

          val className = mkClassName(name)
          s"def ${className}($arg: Array[Double]): Unit$efs = $src"
        }

        println("// Initial code:")
        println(emitSource())


        // // lower zeros, ones, etc to uniform tensor constructor
        // g = (new TensorTransformer("TensorLowering")).transform(g)

        // println("// After Tensor lowering:")
        // println(emitSource())

        // val cg = new CompactScalaCodeGen
        // cg.doRename = true

        // val arg = cg.quote(g.block.in.head)
        // val src = utils.captureOut(cg(g))
        // sc.dumpGeneratedCode = true

        // val className = mkClassName(name)

        // val fc = sc.compile[Int,Int](className, {
        //   s"// Generated code\nclass ${className} extends (Int => Int) {\n def apply($arg: Int): Int = {\n $src\n }\n }"
        // })

        // println("// Output:")

        // println(fc(0))
        // println(fc(1))
        // println(fc(2))
        // println(fc(3))
        // println(fc(4))
      })
    }
  }

  testBE("01") { x =>
    fft(4)(x)
  }


}


/*

class TestFFT extends FileDiffSuite {

  val prefix = home + "test-out/epfl/test2-"

  def testFFT1 = {
    withOutFile(prefix+"fft1") {
      val o = new FFT with ArithExp with TrigExpOpt with FlatResult with DisableCSE //with DisableDCE
      import o._

      val r = fft(List.tabulate(4)(_ => Complex(fresh[Double], fresh[Double])))
      println(globalDefs.mkString("\n"))
      println(r)

      val p = new ExportGraph with DisableDCE { val IR: o.type = o }
      p.emitDepGraph(result[Unit](r), prefix+"fft1-dot", true)
    }
    assertFileEqualsCheck(prefix+"fft1")
    assertFileEqualsCheck(prefix+"fft1-dot")
  }

  def testFFT2 = {
    withOutFile(prefix+"fft2") {
      val o = new FFT with ArithExpOptFFT with TrigExpOptFFT with FlatResult
      import o._

      val r = fft(List.tabulate(4)(_ => Complex(fresh[Double], fresh[Double])))
      println(globalDefs.mkString("\n"))
      println(r)

      val p = new ExportGraph { val IR: o.type = o }
      p.emitDepGraph(result[Unit](r), prefix+"fft2-dot", true)
    }
    assertFileEqualsCheck(prefix+"fft2")
    assertFileEqualsCheck(prefix+"fft2-dot")
  }

  def testFFT3 = {
    withOutFile(prefix+"fft3") {
      class FooBar extends FFT
        with ArithExpOptFFT with TrigExpOptFFT with ArraysExp
        with CompileScala {

        def ffts(input: Rep[Array[Double]], size: Int) = {
          val list = List.tabulate(size)(i => Complex(input(2*i), input(2*i+1)))
          val r = fft(list)
          // make a new array for now - doing in-place update would be better
          makeArray(r.flatMap { case Complex(re,im) => List(re,im) })
        }

        val codegen = new ScalaGenFlat with ScalaGenArith with ScalaGenArrays { val IR: FooBar.this.type = FooBar.this } // TODO: find a better way...
      }
      val o = new FooBar
      import o._

      val fft4 = (input: Rep[Array[Double]]) => ffts(input, 4)
      codegen.emitSource(fft4, "FFT4", new PrintWriter(System.out))
      val fft4c = compile(fft4)
      println(fft4c(Array(1.0,0.0, 1.0,0.0, 2.0,0.0, 2.0,0.0, 1.0,0.0, 1.0,0.0, 0.0,0.0, 0.0,0.0)).mkString(","))
    }
    assertFileEqualsCheck(prefix+"fft3")
  }

*/
