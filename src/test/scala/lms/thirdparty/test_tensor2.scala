package lms
package tensors

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._

import Backend._

class FixedSizeTensorTest extends TutorialFunSuite {
  val under = "tensors"

  abstract class CompilerCTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorFrontEnd {
    override def transform(graph: Graph) = {
      graph.show
      val graph1 = (new TensorLowering2{val frontEnd = new FixedSizeTensorFrontEnd{}; init() }).transform(graph)
      graph1.show
      graph1
    }
  }

  test("show") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val array1 = Array(1,2,3,4,5,6,7,8)
        val tensor1 = Tensor(Seq(2,2,2), Unwrap(array1))
        printf("%d ", tensor1.shape)
        tensor1.show
      }
    }
    System.out.println(indent(driver.code))
  }

  test("basic") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val array1 = Array(1,2,3,4,5,6)
        val tensor1 = Tensor(Seq(2,3), Unwrap(array1))

        val array2 = Array(6,5,4,3,2,1)
        val tensor2 = Tensor(Seq(2,3), Unwrap(array2))
        val tensor3 = tensor1 + tensor2

        printf("%d ", tensor3.shape)
        tensor3.show
      }
    }
    System.out.println(indent(driver.code))
  }

//   test("basic1") {
//     val driver = new CompilerCTensor[Int, Unit] {
//       @virtualize
//       def snippet(arg: Rep[Int]): Rep[Unit] = {
//         val arr1 = Array(1,2,3,4,5)
//         val arr2 = Array(5,4,3,2,1)
//         val tensor1 = Tensor(arr1, 5)
//         val tensor2 = Tensor(arr2, 5)
//         val tensor3 = tensor1 + tensor2
//         tensor3.show()
//       }
//     }
//   }

}
