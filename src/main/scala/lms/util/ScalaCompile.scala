package lms.util

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader

trait ScalaCompile {

  //val codegen: ScalaCodegen { val IR: ScalaCompile.this.type }

  var compiler: Global = _
  var reporter: ConsoleReporter = _
  //var output: ByteArrayOutputStream = _

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
     */
    val settings = new Settings()
    val pathSeparator = System.getProperty("path.separator")

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out)) //writer
    compiler = new Global(settings, reporter)
  }

  var compileCount = 0

  var dumpGeneratedCode = false

  def nextClassName = "staged$" + compileCount

  // NOTE: class name must be unique (e.g. use nextClassName)
  //def compile[A,B](f: Exp[A] => Exp[B])(implicit mA: Typ[A], mB: Typ[B]): A=>B = {
  def compile[A, B](className: String, source: String, staticData: List[(Class[_], Any)]): A => B = {
    if (this.compiler eq null)
      setupCompiler()

    compileCount += 1

    // val source = new StringWriter()
    // val writer = new PrintWriter(source)
    // val staticData = codegen.emitSource(f, className, writer)

    // val staticData: List[({val tp: Manifest[Any]},Any)] = Nil

    // codegen.emitDataStructures(writer)

    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1): _*)

    val obj: A => B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]): _*).asInstanceOf[A => B]
    obj
  }
}
