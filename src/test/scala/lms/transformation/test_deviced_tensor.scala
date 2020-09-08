package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty.{CCodeGenLibs, CCodeGenCudaOps, CCodeGenCBLASOps}

import Backend._


class FixedSizeDevicedTensorTest extends TutorialFunSuite {
  val under = "transformer/deviced_tensor"

  abstract class CompilerCDevicedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeTensorDeviceOps { q =>

    override val codegen = new DslGenC with CCodeGenLibs with CCodeGenCBLASOps with CCodeGenCudaOps {
      val IR: q.type = q
    }

    override def transform(graph: Graph) = {
      graph.show
      val graph1 = (new DevicedTensorLowering {}).transform(graph)
      graph1.show
      graph1
    }
  }

  test("show") {
    val driver = new CompilerCDevicedTensor[Int, Unit] {
      import FixedSizeTensorDeviceTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val tensor1 = Tensor(Seq(2,3), Array[Float](1,2,3,4,5,6))
        val tensor2 = Tensor(Seq(2,3), Array[Float](6,5,4,3,2,1))
        tensor1.show
        tensor2.show
      }
    }
    System.out.println(indent(driver.code))
  }

  test("add") {
    val driver = new CompilerCDevicedTensor[Int, Unit] {
      import FixedSizeTensorDeviceTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val tensor1 = Tensor(Seq(2,3), Array[Float](1,2,3,4,5,6))
        val tensor2 = Tensor(Seq(2,3), Array[Float](6,5,4,3,2,1))
        implicit val device = CPU(0)
        val tensor3 = tensor1 + tensor2
        tensor3.show
      }
    }
    System.out.println(indent(driver.code))
  }

}

