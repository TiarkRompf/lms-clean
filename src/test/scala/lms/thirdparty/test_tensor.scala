package lms
package thirdparty

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import macros.SourceContext
import lms.collection._

class TensorTest extends TutorialFunSuite {
  val under = "thirdparty/tensor"

  abstract class CompilerCTensor[A:Manifest, B:Manifest] extends CompilerC[A, B] with TensorOps1 { q =>

    override def transform(g: Graph) = {
      (new TensorLowering1).transform(g)
    }
  }

  test("basic") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val arr1 = Array(1,2,3,4,5)
        val tensor1 = Tensor(arr1, Seq(5))
        tensor1.show()
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
