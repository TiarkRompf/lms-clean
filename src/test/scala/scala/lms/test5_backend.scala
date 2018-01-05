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

  def testBE(name: String, verbose: Boolean = false)(prog: INT => INT) = {
    test(name) {
      checkOut(name, "scala", {
        val g = program(prog)

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

  testBE("fac-01", verbose = true) { x =>
    val fac = FUN { (f, n) => 
      IF (n) {
        n * f(n-1)
      } {
        1
      }
    }
    fac(x)
  }

  testBE("fac-02", verbose = true) { x =>
    val fac = FUN { (f, n) => 
      IF (n) {
        n * f(n-((2:INT)-1)) // extra stm does not depend on n -> hoist out of fac
      } {
        1
      }
    }
    fac(x)
  }

  testBE("codemotion-01") { x =>
  
    val f = FUN { x =>
      val g = FUN { y =>
        PRINT(1337)
        y + (liftInt(4) * liftInt(3))
      }
      g(1)
    }
    f(2)
  }
  

/*
@virtualize
trait NestCondProg2 extends Arith with Functions with IfThenElse with Print {
  
  /* Previously this program exhibited behavior that is likely undesired in many
  cases. The definition of f was moved *into* g and into the conditional.
  The doLambda in the else branch would not be hoisted out of g either.
  
  Although there are situations where this particular kind of code motion
  is an improvement (namely, if the probability of y == true is very low
  and the else branch would be cheap).
  */
  
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] => 2 * x }
    
    val g = doLambda { y: Rep[Boolean] =>
      print("yo")
      if (y)
        f
      else
        doLambda { x: Rep[Double] => x + 1 }
    }
    g
  }
  
}


@virtualize
trait NestCondProg3 extends Arith with Functions with IfThenElse with Print {
  
  def test(x: Rep[Unit]) = {
    val f = if (unit(true)) doLambda { x: Rep[Double] => 2 * x } else doLambda { x: Rep[Double] => 4 * x }
    
    val g = doLambda { y: Rep[Boolean] =>
      print("yo")
      if (y) {
        print("then")
        f
      } else {
        print("else")
        if (unit(false)) doLambda { x: Rep[Double] => x + 1 } else doLambda { x: Rep[Double] => x + 2 }
      }
    }
    g
  }
  
}

@virtualize
trait NestCondProg4 extends Arith with Functions with IfThenElse with Print {
  
  def test(x: Rep[Unit]) = {
    val g = doLambda { y: Rep[Double] =>
      if (unit(true)) {
        val x = y + 1.0
        print(x)
        ()
      } else {
      }
    }
    g
  }
  
}


@virtualize
trait NestCondProg5 extends Arith with Functions with IfThenElse with Print {
  
  def test(x: Rep[Unit]) = {
    if (unit(true)) {
      // should place 7 + 9 here
      doLambda { y: Rep[Double] =>
        print(unit(7.0) + unit(9.0))
      }
    } else {
    }
  }
  
}

@virtualize
trait NestCondProg6 extends Arith with Functions with IfThenElse with Print {
  
  // FIXME: this one doesn't work yet!!!

  def test(x: Rep[Unit]) = {
    val z = unit(7.0) + unit(9.0) // should move into the conditional (but isn't currently)
    val x = if (unit(true)) {
      print(z)
    } else {
    }
    doLambda { y: Rep[Boolean] => 
      print(x)
    }
  }
  
}

@virtualize
trait NestCondProg7 extends Arith with OrderingOps with Functions with IfThenElse with Print {

  def test(x: Rep[Unit]) = {    
    doLambda { y: Rep[Double] => 
      if (y < 100) {
        val z = y + unit(9.0) // should stay inside conditional: 
                              // apparently z was moved up because it is also used in the lambda (z+u)
        doLambda { u: Rep[Double] =>
          z + u
        }
      } else {
      }
    }
  }
  
}
*/

}