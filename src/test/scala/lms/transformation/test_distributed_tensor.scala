package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty.{CCodeGenLibs, CCodeGenMPI, CCodeGenNCCLOps}
import lms.thirdparty.array_computation.{CCodeGenCBLASOps, CCodeGenCudaOps}

import Backend._


class FixedSizeDistributedTensorTest extends TutorialFunSuite {
  val under = "transformer/distributed_tensor/"

  abstract class CompilerCDistributedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeDistributedTensorOps { q =>

    override val codegen = new DslGenCPP with CCodeGenLibs with CCodeGenCBLASOps with
        CCodeGenCudaOps with CCodeGenNCCLOps with CCodeGenMPI {
      val IR: q.type = q

      override def mayInline(n: Node): Boolean = n match {
        case Node(_, s, _, _) if s.startsWith("tensor") => false
        case _ => super.mayInline(n)
      }
    }

    override val passes = List(
      new DistributeTensorDimName {},
      new DistributeTensorAIRCoP {},
      new DistributedTensorCanonicalize {},
      new DistributeTensor2MPI_NCCL {})

    var log_path: String = ""
    def setLogPath(path: String) { log_path = path }

    override def transform(graph: Graph): Graph = {
      logGraph(graph.toString, log_path)
      super.transform(graph)
    }

    override def transformOnePass(pass: Transformer, index: Int, graph: Graph) = {
      val new_graph = pass.transform(graph)
      if (log_path == "") throw new Exception("should set log_path first")
      logGraph(new_graph.toString, log_path, index, pass.name)
      new_graph
    }
  }

  // test("AD") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0
  //       implicit val anno = NAnno
  //       val model = module {
  //         val tensor_input = Tensor.input[Float](Seq(3, 3))
  //         val tensor_weight = Tensor.weight[Float](Seq(3, 3))
  //         tensor_input * tensor_weight
  //       }
  //       model()
  //       printf("compile")
  //     }
  //   }
  //   check("AD", driver.code, "cu")
  // }

  test("Annotation") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        dim_name = 0
        val inputTensorType = tensor_type[Float](Seq(32,32))
        implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

        val model = module {
          val tensor_input = Tensor.input[Float](inputTensorType)
          val tensor_weight = Tensor.weight[Float](Seq(32, 32))
          tensor_input * (tensor_weight, batchSplitAnno)
        }
        model(10)
        printf("compile\n")
      }
    }
    checkWithLogPath("Annotation", driver.code, "cu", driver.setLogPath)
  }

  test("split") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        dim_name = 0
        val inputTensorType = tensor_type[Float](Seq(32, 32))
        implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

        val model = module {
          val tensor_input = Tensor.input[Float](inputTensorType)
          val tensor_weight = Tensor.weight[Float](Seq(32, 16))
          val splits = tensor_input.split(1, List(16, 16), batchSplitAnno)
          splits(0) * (tensor_weight, batchSplitAnno)
        }
        model(10)
        printf("compile\n")
      }
    }
    checkWithLogPath("split", driver.code, "cu", driver.setLogPath)
  }

  test("split2") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        dim_name = 0
        val inputTensorType = tensor_type[Float](Seq(32, 32))
        implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

        val model = module {
          val tensor_input = Tensor.input[Float](inputTensorType)
          val tensor_weight = Tensor.weight[Float](Seq(32, 64))
          val splits = tensor_weight.split(1, List(32, 32), batchSplitAnno)
          tensor_input * (splits(0), batchSplitAnno)
        }
        model(10)
        printf("compile\n")
      }
    }
    checkWithLogPath("split2", driver.code, "cu", driver.setLogPath)
  }

  test("dot") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        dim_name = 0
        val inputTensorType = tensor_type[Float](Seq(32,32))
        implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

        val model = module {
          val tensor_input = Tensor.input[Float](inputTensorType)
          val tensor_weight = Tensor.weight[Float](Seq(32, 32))
          tensor_input gemm (tensor_weight, batchSplitAnno)
        }
        model(10)
        printf("compile\n")
      }
    }
    checkWithLogPath("dot", driver.code, "cu", driver.setLogPath)
  }

  // test("show") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0
  //       val gpus = List(GPU(0), GPU(1))
  //       val a = INPUT(Seq(32,64), manifest[Float], 1, gpus)
  //       val b = INPUT(Seq(32,64), manifest[Float], 1, gpus)
  //       val c = Add(a, b, a.annotation)
  //       c.show
  //       ()
  //     }
  //   }
  //   System.out.println(indent(driver.code))
  // }

}

