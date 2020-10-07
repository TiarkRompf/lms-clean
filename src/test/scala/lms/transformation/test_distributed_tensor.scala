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


class FixedSizeDistributedTensorTest extends TutorialFunSuite {
  val under = "transformer/distributed_tensor/"

  abstract class CompilerCDistributedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeDistributedTensorOps { q =>

    override val codegen = new DslGenCPP with CCodeGenLibs with CCodeGenCBLASOps with CCodeGenCudaOps {
      val IR: q.type = q

      override def mayInline(n: Node): Boolean = n match {
        case Node(_, s, _, _) if s.startsWith("tensor") => false
        case _ => super.mayInline(n)
      }
    }

    override def transform(graph: Graph): Graph = {
      graph.show
      val graph1 = (new DistributeTensorDimName {}).transform(graph)
      graph1.show
      val graph2 = (new DistributeTensorAIRCoP {}).transform(graph1)
      graph2.show
      graph2
    }
  }

  test("AD") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        implicit val anno = NAnno
        val model = module { () =>
          val tensor_input = Tensor.input[Float](Seq(3, 3))
          val tensor_weight = Tensor.weight[Float](Seq(3, 3))
          tensor_input * tensor_weight
        }
        model()
        printf("compile")
      }
    }
    check("AD", driver.code, "cu")
  }

  test("Annotation") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val inputTensorType = tensor_type[Float](Seq(32,32))
        implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

        val model = module { () =>
          val tensor_input = Tensor.input[Float](inputTensorType)
          val tensor_weight = Tensor.weight[Float](Seq(32, 32))
          tensor_input * (tensor_weight, batchSplitAnno)
        }
        model()
        printf("compile")
      }
    }
    check("Annotation", driver.code, "cu")
  }

  test("dot") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val inputTensorType = tensor_type[Float](Seq(32,32))
        implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

        val model = module { () =>
          val tensor_input = Tensor.input[Float](inputTensorType)
          val tensor_weight = Tensor.weight[Float](Seq(32, 32))
          tensor_input dot (tensor_weight, batchSplitAnno)
        }
        model()
        printf("compile")
      }
    }
    check("dot", driver.code, "cu")
  }

  test("show") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val gpus = List(GPU(0), GPU(1))
        val a = INPUT(Seq(32,64), manifest[Float], 1, gpus)
        val b = INPUT(Seq(32,64), manifest[Float], 1, gpus)
        val c = a + (b, a.annotation)
        c.show
        ()
      }
    }
    System.out.println(indent(driver.code))
  }

}

