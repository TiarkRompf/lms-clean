package lms
package transformation

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._

import Backend._

class FixedSizeTensorTest extends TutorialFunSuite {
  val under = "transformer/tensor2"

  abstract class CompilerCTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorOps {
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

}
