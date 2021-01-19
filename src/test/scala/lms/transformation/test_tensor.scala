package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty.{CCodeGenLibs}
import lms.thirdparty.array_computation.{CCodeGenCBLASOps}

import Backend._

class FixedSizeTensorTest extends TutorialFunSuite {
  val under = "transformer/tensor2/"

  abstract class CompilerCTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorOps { q =>

    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCBLASOps {
      val IR: q.type = q
    }

    override val passes = List(new TensorLoweringCPU {})
  }

  test("show") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val tensor = Tensor(Seq(2,2,2), Array(1,2,3,4,5,6,7,8))
        printf("%d ", tensor.shape)
        tensor.show
      }
    }
    check("show", driver.code, "c")
  }

  test("tensor_construction_effect") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val array = NewArray[Int](6)
        for (i <- (0 until 6): Rep[Range]) {
          array(i) = i
        }
        val tensor = Tensor(Seq(2,3), array)
        tensor.show
      }
    }
    check("tensor_construction_effect", driver.code, "c")
  }

  test("add") {
    val driver = new CompilerCTensor[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val tensor1 = Tensor[Int](Seq(2,3), Array(1,2,3,4,5,6))
        val tensor2 = Tensor(Seq(2,3), Array(6,5,4,3,2,1))
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
    check("eleBinary", driver.code, "c")
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
    check("dot", driver.code, "c")
  }

}
