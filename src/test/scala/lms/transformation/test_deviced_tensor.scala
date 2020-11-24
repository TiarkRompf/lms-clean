package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty.{CCodeGenLibs}
import lms.thirdparty.array_computation.{CCodeGenCBLASOps, CCodeGenCudaOps}

import Backend._


class FixedSizeDevicedTensorTest extends TutorialFunSuite {
  val under = "transformer/deviced_tensor/"

  abstract class CompilerCDevicedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorDeviceOps { q =>

    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCBLASOps with CCodeGenCudaOps {
      val IR: q.type = q
    }

    override def transform(graph: Graph) = {
      val graph1 = (new TensorResolvingDevice {}).transform(graph)
      val graph2 = (new DevicedTensorLowering {}).transform(graph1)
      List(graph, graph1, graph2)
    }
  }

  test("show") {
    val driver = new CompilerCDevicedTensor[Int, Unit] {
      import FixedSizeTensorDeviceTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val array = NewArray[Float](6, GPU(0))
        // FIXME(feiw) slightly not ideal since GPU(0) is given to both array and Tensor
        val tensor1 = Tensor(Seq(2,3), array, GPU(0))
        tensor1.show
      }
    }
    check("show", driver.code, "cu")
  }

  test("add") {
    val driver = new CompilerCDevicedTensor[Int, Unit] {
      import FixedSizeTensorDeviceTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val tensor1 = Tensor(Seq(2,3), Array[Float](1,2,3,4,5,6))
        val tensor2 = Tensor(Seq(2,3), Array[Float](6,5,4,3,2,1))
        val tensor3 = atGPU { implicit gpu =>
          tensor1 + tensor2
        }
        tensor3.show
      }
    }
    check("add", driver.code, "cu")
  }

  test("binary") {
    val driver = new CompilerCDevicedTensor[Int, Unit] {
      import FixedSizeTensorDeviceTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val tensor1 = Tensor(Seq(2,3), Array[Float](1,2,3,4,5,6))
        val tensor2 = Tensor(Seq(2,3), Array[Float](6,5,4,3,2,1))
        val Seq(tensor3, tensor4, tensor5, tensor6) = atGPU { implicit gpu =>
          Seq(tensor1 + tensor2,
              tensor1 - tensor2,
              tensor1 * tensor2,
              tensor1 / tensor2)
        }
        tensor3.show
        tensor4.show
        tensor5.show
        tensor6.show
      }
    }
    check("binary", driver.code, "cu")
  }

}

