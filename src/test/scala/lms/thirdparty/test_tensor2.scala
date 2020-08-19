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

  // first: get a driver :)
  abstract class CompilerCTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorFrontEnd {
    override def transform(graph: Graph) = {
      (new TensorShow{ val frontEnd = new FixedSizeTensorFrontEnd{}; init() }).transform(graph)
    }
  }

  test("basic") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        // FIXME(feiw) handle this `g` better?
        g = Adapter.g

        val array = Array(1,2,3,4,5)
        val tensor1 = Tensor(Seq(5), Unwrap(array))
        printf("%d ", tensor1.shape)
        tensor1.show
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
