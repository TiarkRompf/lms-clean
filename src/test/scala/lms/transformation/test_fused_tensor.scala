package lms
package transformation.tensor

import scala.annotation.implicitNotFound
import lms.core.virtualize
import macros.SourceContext

import lms.core._
import lms.core.stub._
import lms.thirdparty.{CCodeGenLibs}
import lms.thirdparty.array_computation.{CCodeGenCBLASOps, CCodeGenCudaOps, CCodeGenCuBLAS}

import Backend._

class FixedSizeFusedTensorTest extends TutorialFunSuite {
  val under = "transformer/fused_tensor/"

  abstract class CompilerCFusedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FusedTensorOps { q =>

    override val codegen = new DslGenCPP with CCodeGenCudaOps with CCodeGenLibs {
      val IR: q.type = q
    }

    override val passes = List(
      new FusedTensorFunctional {},
      // new FusedTensorSplit {},
      // new FusedTensorSimplify {},
      // new FusedTensorConcat {},
      // new Canonicalize {},
      new FusedTensorVertical {},
      new Canonicalize {},
      new FusedTensorToCuda {},
    )

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
        // node.toString
        stream.println(node)
      stream.println(graph.block)
      stream.println("==================")
      source.toString
    }
  }

  test("split") {
    val driver = new CompilerCFusedTensor[Int, Unit] {
      import FusedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val a = Tensor.input[Int](10)
        val b = Tensor.ones[Int](5)

        val c = a.split(Seq(5, 5))
        val d = c.result(0)
        val e = c.result(1)

        val f = d + b
        val g = f.concat(e)
        g.show
      }
    }
    checkWithLogPath("split", driver.code, "cu", driver.setLogPath)
  }

  test("add") {
    val driver = new CompilerCFusedTensor[Int, Unit] {
      import FusedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val a = Tensor.input[Int](10)
        val b = Tensor.zeros[Int](10)
        val c = Tensor.ones[Int](10)
        val d = a + b
        val e = a - c
        // printf("%d", c(0))
        // printf("%d", d(0))
        d.show
        e.show
      }
    }
    checkWithLogPath("add", driver.code, "cu", driver.setLogPath)
  }

  test("tanh") {
    val driver = new CompilerCFusedTensor[Int, Unit] {
      import FusedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val a = Tensor.input[Int](10)
        val c = a.tanh
        c.show
      }
    }
    checkWithLogPath("tanh", driver.code, "cu", driver.setLogPath)
  }
  
  test("relu") {
    val driver = new CompilerCFusedTensor[Int, Unit] {
      import FusedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val a = Tensor.input[Int](10)
        val b = a.relu
        b.show
      }
    }
    checkWithLogPath("relu", driver.code, "cu", driver.setLogPath)
  }

  test("input") {
    val driver = new CompilerCFusedTensor[Int, Unit] {
      import FusedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val a = Tensor.input[Int](10)
        val b = Tensor.ones[Int](10)
        val c = a + b
        c.show
      }
    }
    checkWithLogPath("input", driver.code, "cu", driver.setLogPath)
  }
}