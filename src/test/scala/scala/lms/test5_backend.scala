package scala.lms

import scala.annotation.implicitNotFound

class BackendTest extends TutorialFunSuite {
  val under = "backend-"

  val fe = new FrontEnd
  import fe._

  val sc = new internal.ScalaCompile {}
  sc.dumpGeneratedCode = true

  def mkClassName(name: String) = {
    // mangle class name
    (under + name).replace("-","_")
  }

  def testBE(name: String)(prog: INT => INT) = {
    test(name) {
      checkOut(name, "scala", {
        val g = program(prog)

        println("// Raw:")
        g.nodes.foreach(println)

        println("// Generic Codegen:")
        (new CodeGen)(g)

        println("// Scala Codegen:")
        (new ScalaCodeGen)(g)

        println("// Compact Scala Codegen:")
        (new CompactScalaCodeGen)(g)

        val cg = new CompactScalaCodeGen

        val arg = cg.quote(g.block.in.head)
        val src = utils.captureOut(cg(g))
        sc.dumpGeneratedCode = true

        val className = mkClassName(name)

        val fc = sc.compile[Int,Int](className, {
          s"// Generated code\nclass ${className} extends (Int => Int) {\n def apply($arg: Int): Int = {\n $src\n }\n }"
        })

        println("// Output:")

        println(fc(0))
        println(fc(1))
        println(fc(2))
        println(fc(3))
        println(fc(4))
      })
    }
  }

  // basic scheduling and code motion tests

  testBE("fac-01") { x =>
    val fac = FUN { (f, n) => 
      IF (n) {
        n * f(n-1)
      } {
        1
      }
    }
    fac(x)
  }

  testBE("fac-02") { x =>
    val fac = FUN { (f, n) => 
      IF (n) {
        n * f(n-((2:INT)-1)) // extra stm does not depend on n -> hoist out of fac
      } {
        1
      }
    }
    fac(x)
  }

}