package lms
package thirdparty

import lms.core.stub._
import lms.core.virtualize
import macros.SourceContext
import lms.collection._

class ScannerTest extends TutorialFunSuite {
  val under = "thirdparty/scanner"

  abstract class DslDriverCScanner[A: Manifest, B: Manifest] extends DslDriverC[A,B] with ScannerOps { q =>
    override val codegen = new DslGenC with CCodeGenScannerOps {
      val IR: q.type = q
    }
  }

  test("open") {
    val driver = new DslDriverCScanner[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val lanternPath = System.getProperty("user.dir")
        val filename = lanternPath + "/src/test/scala/lms/thirdparty/test_scanner.scala"
        val fd = open(filename)
        val filelength = filelen(fd)
        printf("file length is %ld\n", filelength)
        close(fd)
      }
    }
    System.out.println(indent(driver.code))
    driver.eval(0)
  }

  test("mmap") {
    val driver = new DslDriverCScanner[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val lanternPath = System.getProperty("user.dir")
        val filename = lanternPath + "/src/test/scala/lms/thirdparty/test_scanner.scala"
        val fd = open(filename)
        val array = mmap[Char](fd, unit(50l))
        for (i <- (0 until 50): Rep[Range])
          printf("%c", array(i))
        close(fd)
      }
    }
    System.out.println(indent(driver.code))
    driver.eval(0)
  }

  test("a") {
    val driver = new DslDriverC[Int, Unit] with MyF {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        myF(arg)
      }
    }
    System.out.println(driver.code)
  }

  test("openf") {
    val driver = new DslDriverCScanner[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val lanternPath = System.getProperty("user.dir")
        val filename = lanternPath + "/src/test/scala/lms/thirdparty/test_binary"
        val fp = openf(filename, "w")
        fprintf(fp, "%d", 10)
        closef(fp)

        val fp2 = openf(filename, "r")
        var target = 0
        getInt(fp2, target)
        printf("%d", target)
        closef(fp2)
      }
    }
    System.out.println(indent(driver.code))
    driver.eval(0)
  }
}


