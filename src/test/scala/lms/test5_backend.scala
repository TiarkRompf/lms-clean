package lms

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

  def testBE(name: String, verbose: Boolean = false)(prog: INT => INT) = {
    test(name) {
      checkOut(name, "scala", {
        var g = program(prog)

        if (verbose) {
          println("// Raw:")
          g.nodes.foreach(println)

          println("// Generic Codegen:")
          (new CodeGen)(g)

          println("// Scala Codegen:")
          (new ScalaCodeGen)(g)

          println("// Compact Scala Codegen:")
          (new CompactScalaCodeGen)(g)
        }

        // identity transformer -- just to verify it works
        g = (new Transformer { g = new GraphBuilder }).transform(g)

        val cg = new CompactScalaCodeGen
        cg.doRename = true

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

  testBE("fac-01", verbose = true) { x =>
    val fac = FUN { (f, n) => 
      IF (n !== 0) {
        n * f(n-1)
      } {
        1
      }
    }
    fac(x)
  }

  testBE("fac-02", verbose = true) { x =>
    val fac = FUN { (f, n) => 
      IF (n !== 0) {
        n * f(n-((2:INT)-1)) // extra stm does not depend on n -> hoist out of fac
      } {
        1
      }
    }
    fac(x)
  }

  // nested additive frequency issue

  testBE("codemotion-00") { x =>
  
    val f = FUN { x =>
      IF(true) { 
        (3:INT) + (4:INT)
      } {
        (5:INT) * (6:INT)
      }
    }
    f(2)
  }

  // Test cases from LMS repo follow

  testBE("codemotion-01") { x =>
  
    val f = FUN { x =>
      val g = FUN { y =>
        PRINT("1337")
        y + (liftInt(4) * liftInt(3))
      }
      g(1)
    }
    f(2)
  }
  

  /* Previously this program exhibited behavior that is likely undesired in many
  cases. The definition of f was moved *into* g and into the conditional.
  The doLambda in the else branch would not be hoisted out of g either.
  
  Although there are situations where this particular kind of code motion
  is an improvement (namely, if the probability of y == true is very low
  and the else branch would be cheap).
  */
  
  
  testBE("codemotion-02") { x =>
    val f = FUN { x => 2 * x }
    
    val g = FUN { y =>
      PRINT("1337")
      IF (y !== 0) {
        f(1)
      } /*else*/ {
        (FUN { x => x + 1 })(2)
      }
    }
    g(3)
  }
  
  testBE("codemotion-03") { x =>
    // Both IFs hoisted, FUNs inside branches

    val f = IF (true) { (FUN { x => 2 * x })(1) } /*else*/ { (FUN { x => 4 * x })(2) }
    
    val g = FUN { y =>
      PRINT("1337")
      IF (y !== 0) {
        PRINT(1)
        f
      } /*else*/ {
        PRINT(0)
        IF (false) { (FUN { x => x + 1 })(3) } /*else*/ { (FUN { x => x + 2 })(4) }
      }
    }
    g(5)
  }
  
  testBE("codemotion-04") { x =>
  
    val g = FUN { y =>
      IF (true) {
        val x = y + 1
        PRINT(x)
        0
      } /*else*/ {
        0
      }
    }
    g(1)
  }
  

  testBE("codemotion-05") { x =>
    IF (true) {
      // should place 7 + 9 here
      (FUN { y =>
        PRINT((7:INT) + (9:INT))
        0
      })(0)
    } /*else*/ {
      0
    }
  }

  
  // this one didn't work in LMS
  testBE("codemotion-06") { x =>
    val z = (7:INT) + (9:INT) // should move into the conditional (isn't in LMS)
    val x = IF (true) {
      PRINT(z)
      0
    } /*else*/ {
      0
    }
    (FUN { y => 
      PRINT(x)
      0
    })(1)
  }
  
  testBE("codemotion-07") { x =>
    val f = FUN { y  => 
      IF (y !== 0) {
        val z = y + 9 // should stay inside conditional: 
                      // apparently z was moved up because it is also used in the lambda (z+u)
        val g = FUN { u =>
          z + u
        }
        g(0)
      } /*else*/ {
        0
      }
    }
    f(1)
  }
  
  // loops, vars, and arrays

  testBE("loops-01") { x =>
    val as = ARRAY(x)
    val i = VAR(0)
    val s = VAR(0)
    WHILE { i() !== x } { // why does i !== x compile? ScalaTest?
      s() = s() + i()
      as(i()) = s()
      i() = i() + 1
    }
    s()
  }

  testBE("reorder-01") { x =>
    val i = VAR(x)
    val read = i()
    i() = 10
    read
  }

  testBE("reorder-02") { x =>
    val i = VAR(x)
    val read = i()
    val add = read + 5
    i() = 10
    add
  }

  testBE("reorder-03") { x =>
    val i = VAR(x)
    val read1 = i()
    val read2 = i()
    val add = read1 + read2
    add // should inline
  }

  testBE("reorder-04") { x =>
    val i = VAR(x)
    val read1 = i()
    val read2 = i()
    val add = read2 + read1
    add // should not inline
  }

}
