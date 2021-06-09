package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq
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
      logGraph(show_graph(graph), log_path)
      super.transform(graph)
    }

    override def transformOnePass(pass: Transformer, index: Int, graph: Graph) = {
      val new_graph = pass.transform(graph)
      if (log_path == "") throw new Exception("should set log_path first")
      logGraph(show_graph(new_graph), log_path, index, pass.name)
      new_graph
    }

    def show_graph(graph: Graph): String = {
      // return a string representation of the graph
      val source = new java.io.ByteArrayOutputStream()
      val stream = new java.io.PrintStream(source)
      stream.println("==================")
      for (node <- graph.nodes)
        stream.println(showTensor(node, graph))
      stream.println(graph.block)
      stream.println("==================")
      source.toString
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

  test("invert") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32,32), tensorName=Some("weight"))
          input + weight.inv
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("invert", driver.code, "cu", driver.setLogPath)
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

  test("split_small") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val splits = input.split(1, List(16,16))
          splits(0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split_small", driver.code, "cu", driver.setLogPath)
  }

  test("split_small3D") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val splits = input.split(2, List(16,16))
          splits(0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split_small3D", driver.code, "cu", driver.setLogPath)
  }

  test("split") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32,16), tensorName=Some("weight"))
          val splits = input.split(1, List(16,16))
          splits(0) * weight
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split", driver.code, "cu", driver.setLogPath)
  }

  test("split3D") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32,32,16), tensorName=Some("weight"))
          val splits = input.split(2, List(16,16))
          splits(0) * weight
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split3D", driver.code, "cu", driver.setLogPath)
  }

  test("split2") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,16), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32,32), tensorName=Some("weight"))
          val splits = weight.split(1, List(16,16))
          input * splits(0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split2", driver.code, "cu", driver.setLogPath)
  }

  test("split2_3D") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32,16), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32,32,32), tensorName=Some("weight"))
          val splits = weight.split(2, List(16,16))
          input * splits(0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split2_3D", driver.code, "cu", driver.setLogPath)
  }

  test("split4_3D") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32,8), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(32,32,32), tensorName=Some("weight"))
          val splits = weight.split(2, List(8,8,8,8))
          input * splits(0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("split4_3D", driver.code, "cu", driver.setLogPath)
  }

  test("relu") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          input.relu
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("relu", driver.code, "cu", driver.setLogPath)
  }

  test("tanh") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          input.tanh
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("tanh", driver.code, "cu", driver.setLogPath)
  }

  // test("transpose") {
  //   val driver = new CompilerCDistributedTensor[Int, Unit] {
  //     import FixedSizeDistributedTensorTypeLess._

  //     @virtualize
  //     def snippet(arg: Rep[Int]): Rep[Unit] = {
  //       val model = module {
  //         val input = Tensor.input[Float](shape=Seq(32,32), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
  //         implicit val anno = input.anno
  //         input.trans
  //       }
  //       model.test("loss"); ()
  //     }
  //   }
  //   checkWithLogPath("transpose", driver.code, "cu", driver.setLogPath)
  // }

  test("softmax") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,3,3), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,3,3), tensorName=Some("weight"))

          val params = SoftmaxParam(1.0f, 0.0f)
          input + weight.softmax(params)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("softmax", driver.code, "cu", driver.setLogPath)
  }

  test("sigmoid") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,3,3), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,3,3), tensorName=Some("weight"))
          input + weight.sigmoid()
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("sigmoid", driver.code, "cu", driver.setLogPath)
  }

  test("elu") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,3,3), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,3,3), tensorName=Some("weight"))
          input + weight.elu(1.0f)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("elu", driver.code, "cu", driver.setLogPath)
  }

  test("dropout") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {

        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,3,3), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,3,3), tensorName=Some("weight"))

          val params = DropoutParam(0.0f, 1)
          val dropouts = weight.dropout(params)
          input + dropouts(0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("dropout", driver.code, "cu", driver.setLogPath)
  }

  test("maxpool") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,9,9), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,9,9), tensorName=Some("weight"))
                                                // window  padding   stride
          val params = PoolingParam(1.0f, 0.0f, Seq(3, 3), Seq(1, 1), Seq(1, 1))
          input + weight.maxpool(params)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("maxpool", driver.code, "cu", driver.setLogPath)
  }

  test("avgpool") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,9,9), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,9,9), tensorName=Some("weight"))
                                                // window  padding   stride
          val params = PoolingParam(1.0f, 0.0f, Seq(3, 3), Seq(1, 1), Seq(1, 1))
          input + weight.avgpool(params)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("avgpool", driver.code, "cu", driver.setLogPath)
  }

  test("conv") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,9,9), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,3,3), tensorName=Some("weight"))
                                            // padding   stride    dilation
          val params = ConvParam(1.0f, 0.0f, Seq(1, 1), Seq(1, 1), Seq(1, 1))
          input.conv(weight, params)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("conv", driver.code, "cu", driver.setLogPath)
  }

  test("conv_train") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,9,9), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,3,3), tensorName=Some("weight"))
                                            // padding   stride    dilation
          val params = ConvParam(1.0f, 0.0f, Seq(1, 1), Seq(1, 1), Seq(1, 1))
          input.conv(weight, params)
        }
        model.train(10); ()
      }
    }
    checkWithLogPath("conv_train", driver.code, "cu", driver.setLogPath)
  }

  test("masked_fill") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,9,9), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val mask = Tensor.input[Int](shape=Seq(2,1,9,9), name="mask", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          val weight = Tensor.weight[Float](Seq(2,1,9,9), tensorName=Some("weight"))

          input + weight.maskedFill(mask, 1.0)
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("masked_fill", driver.code, "cu", driver.setLogPath)
  }

  test("logSoftmax") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
          val input = Tensor.input[Float](shape=Seq(2,1,32,533), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          val weight = Tensor.weight[Float](Seq(2,1,32,533), tensorName=Some("weight"))

          input + weight.logSoftmax
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("logSoftmax", driver.code, "cu", driver.setLogPath)
  }

  test("transpose") { // passed
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
        val input = Tensor.input[Float](shape=Seq(214,56), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          input.trans
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("transpose", driver.code, "cu", driver.setLogPath)
  }

  test("permute") {
    val driver = new CompilerCDistributedTensor[Int, Unit] {
      import FixedSizeDistributedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val model = module {
        val input = Tensor.input[Float](shape=Seq(4,9,9), name="input", splitDim=0, splitTo=List(GPU(0), GPU(1)))
          implicit val anno = input.anno
          input.permute(List(2,0,1))
        }
        model.test("loss"); ()
      }
    }
    checkWithLogPath("permute", driver.code, "cu", driver.setLogPath)
  }
}

