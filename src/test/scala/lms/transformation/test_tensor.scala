package lms
package transformation

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty._

import Backend._

class FixedSizeTensorTest extends TutorialFunSuite {
  val under = "transformer/tensor2"

  abstract class CompilerCTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorFrontEnd { q =>

    override val codegen = new DslGenC with thirdparty.CCodeGenLibs {
      val IR: q.type = q
      override def emitAll(ng: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {

        registerHeader("/opt/OpenBLAS/include", "<cblas.h>")
        registerLibrary("-lopenblas")

        super.emitAll(ng, name)(m1, m2)
      }
    }

    override def transform(graph: Graph) = {
      graph.show
      val graph1 = (new TensorLoweringCPU {}).transform(graph)
      graph1.show
      graph1
    }
  }

  test("show") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val array1 = Array(1,2,3,4,5,6,7,8)
        val tensor1 = Tensor(Seq(2,2,2), array1)
        printf("%d ", tensor1.shape)
        tensor1.show
      }
    }
    check("show", driver.code, "c")
  }

  test("add") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val array1 = Array(1,2,3,4,5,6)
        val tensor1 = Tensor[Int](Seq(2,3), array1)

        val array2 = Array(6,5,4,3,2,1)
        val tensor2 = Tensor(Seq(2,3), array2)
        val tensor3 = tensor1 + tensor2

        printf("%d ", tensor3.shape)
        tensor3.show
      }
    }
    check("add", driver.code, "c")
  }

  test("eleBinary") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val tensor1 = Tensor[Int](Seq(2,3), Array(1,2,3,4,5,6))
        val tensor2 = Tensor[Int](Seq(2,3), Array(6,5,4,3,2,1))

        val tensor3 = tensor1 + tensor2
        tensor3.show

        val tensor4 = tensor1 - tensor2
        tensor4.show

        val tensor5 = tensor1 * tensor2
        tensor5.show

        val tensor6 = tensor1 / tensor2
        tensor6.show
      }
    }
    System.out.println(indent(driver.code))
    driver.eval(0)
  }

  test("dot") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val tensor1 = Tensor[Float](Seq(3), Array[Float](1,2,3))
        val tensor2 = Tensor[Float](Seq(2, 3), Array[Float](1,2,3,4,5,6))
        val tensor3 = Tensor[Float](Seq(3, 2), Array[Float](1,2,3,4,5,6))

        val vv = tensor1 dot tensor1
        vv.show

        val mv = tensor2 dot tensor1
        mv.show

        val mm = tensor2 dot tensor3
        mm.show
      }
    }
    System.out.println(indent(driver.code))
    driver.eval(0)
  }

}
