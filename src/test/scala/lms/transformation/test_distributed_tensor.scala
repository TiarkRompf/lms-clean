package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty.{CCodeGenLibs, CCodeGenMPI, CCodeGenNCCLOps, CCodeGenCUDNN, CCodeGenScannerOps}
import lms.thirdparty.array_computation.{CCodeGenCBLASOps, CCodeGenCudaOps, CCodeGenCuBLAS}

import Backend._


class FixedSizeDistributedTensorTest extends TutorialFunSuite {
  val under = "transformer/distributed_tensor/"

  abstract class CompilerCDistributedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FixedSizeDistributedTensorOps { q =>

    override val codegen = new DslGenCPP with CCodeGenLibs with CCodeGenCBLASOps with
        CCodeGenCudaOps with CCodeGenNCCLOps with CCodeGenMPI with CCodeGenCuBLAS with CCodeGenCUDNN with CCodeGenScannerOps {
      val IR: q.type = q

      override def mayInline(n: Node): Boolean = n match {
        case Node(_, s, _, _) if s.startsWith("tensor_") || s.startsWith("tensors_") => false
        case _ => super.mayInline(n)
      }
    }

    override val passes = List(
      new DistributeTensorDimName {},
      new DistributeTensorAIRCoP {},
      new Canonicalize {},
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

  test("dot") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,13), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(13,16), tensorName=Some("weight"))
          input gemm weight
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("dot", driver.code, "cu", driver.setLogPath)
  }

  test("mult") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          // this is still hacky :(
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32, 32), tensorName=Some("weight"))
          input * weight
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("mult", driver.code, "cu", driver.setLogPath)
  }

  test("negate") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32, 32), tensorName=Some("weight"))
          input + weight.neg
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("negate", driver.code, "cu", driver.setLogPath)
  }

  // test("invert") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0
  //       val inputTensorType = resultType[Float](Seq(32, 32))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_weight = Tensor.weight[Float](Seq(32, 32))
  //         val tensor_intermediate = tensor_weight inv (batchSplitAnno)
  //         tensor_input + (tensor_intermediate, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile\n")
  //     }
  //   }
  //   checkWithLogPath("invert", driver.code, "cu", driver.setLogPath)
  // }

  // test("tanh") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0
  //       val inputTensorType = resultType[Float](Seq(32, 32))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_weight = Tensor.weight[Float](Seq(32, 32))
  //         val tensor_intermediate = tensor_weight tanh (batchSplitAnno)
  //         tensor_input + (tensor_intermediate, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile\n")
  //     }
  //   }
  //   checkWithLogPath("tanh", driver.code, "cu", driver.setLogPath)
  // }

  // test("relu") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0
  //       val inputTensorType = resultType[Float](Seq(32, 32))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_weight = Tensor.weight[Float](Seq(32, 32))
  //         val tensor_intermediate = tensor_weight relu (batchSplitAnno)
  //         tensor_input + (tensor_intermediate, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile\n")
  //     }
  //   }
  //   checkWithLogPath("relu", driver.code, "cu", driver.setLogPath)
  // }

  // test("transpose") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0
  //       val inputTensorType = resultType[Float](Seq(32, 32))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_weight = Tensor.weight[Float](Seq(32, 32))
  //         val tensor_intermediate = tensor_weight trans (batchSplitAnno)
  //         tensor_input + (tensor_intermediate, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile\n")
  //     }
  //   }
  //   checkWithLogPath("transpose", driver.code, "cu", driver.setLogPath)
  // }

  // test("show") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       val inputTensorType = resultType[Float](Seq(32, 32))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_weight = Tensor.weight[Float](Seq(32, 16))
  //         val splits = tensor_input.split(1, List(16, 16), batchSplitAnno)
  //         splits(0) * (tensor_weight, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile\n")
  //     }
  //   }
  //   checkWithLogPath("split", driver.code, "cu", driver.setLogPath)
  // }

  // test("split2") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       val inputTensorType = resultType[Float](Seq(32, 32))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1)))

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_weight = Tensor.weight[Float](Seq(32, 64))
  //         val splits = tensor_weight.split(1, List(32, 32), batchSplitAnno)
  //         tensor_input * (splits(0), batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile\n")
  //     }
  //   }
  //   checkWithLogPath("split2", driver.code, "cu", driver.setLogPath)
  // }

  // test("softmax") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._
  //     import scala.collection.immutable.Seq

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0

  //       val inputTensorType = resultType[Float](Seq(2, 1, 3, 3))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1))) // split the channel dimension

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_filter = Tensor.weight[Float](Seq(2, 1, 3, 3))

  //         val params = SoftmaxParam(1.0f, 0.0f)
  //         val x = tensor_filter softmax (batchSplitAnno, params)
  //         tensor_input + (x, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile")
  //     }
  //   }
  //   checkWithLogPath("softmax", driver.code, "cu", driver.setLogPath)
  // }

  // test("activation") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._
  //     import scala.collection.immutable.Seq

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0

  //       val inputTensorType = resultType[Float](Seq(2, 1, 3, 3))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1))) // split the channel dimension

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_filter = Tensor.weight[Float](Seq(2, 1, 3, 3))

  //         val x = tensor_filter sigmoid (batchSplitAnno)
  //         tensor_input + (x, batchSplitAnno)
  //       }
  //       model(10)
  //       printf("compile")
  //     }
  //   }
  //   checkWithLogPath("activation", driver.code, "cu", driver.setLogPath)
  // }

  // test("dropout") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._
  //     import scala.collection.immutable.Seq

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0

  //       val inputTensorType = resultType[Float](Seq(2, 1, 3, 3))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1))) // split the channel dimension

  //       val model = module {
  //         val tensor_input = Tensor.input[Float](inputTensorType)
  //         val tensor_filter = Tensor.weight[Float](Seq(2, 1, 3, 3))

  //         val params = DropoutParam(0.5f, 1)
  //         val dropouts = tensor_filter dropout (batchSplitAnno, params)

  //         dropouts(0)
  //       }
  //       model(10)
  //       printf("compile")
  //     }
  //   }
  //   checkWithLogPath("dropout", driver.code, "cu", driver.setLogPath)
  // }

  // test("pooling") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._
  //     import scala.collection.immutable.Seq

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       dim_name = 0

  //       val inputTensorType = resultType[Float](Seq(2, 1, 3, 3))
  //       implicit val batchSplitAnno = SAnno(inputTensorType.shape(0).dim, List(GPU(0), GPU(1))) // split the channel dimension
  //       val params = PoolingParam(1.0f, 0.0f, Seq(2, 2), Seq(1, 1), Seq(1, 1))

  //       val model = module {
  //         val tensor_filter = Tensor.weight[Float](Seq(2, 1, 9, 9))

  //         tensor_filter maxpool (batchSplitAnno, params)
  //       }
  //       model(10)
  //       printf("compile")
  //     }
  //   }
  //   checkWithLogPath("pooling", driver.code, "cu", driver.setLogPath)
  // }
}

