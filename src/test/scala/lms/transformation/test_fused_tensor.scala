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

class FixedSizeFusedTensorTest extends TutorialFunSuite {
  val under = "transformer/fused_tensor"

  abstract class CompilerCFusedTensor[A: Manifest, B: Manifest] extends CompilerC[A,B] with FusedTensorOps { q =>

    override val codegen = new DslGenC with CCodeGenLibs {
      val IR: q.type = q
    }

    override val passes = List(
      new FusedTensorLowering {},
      new FusedTensorVertical {}
    )

    /*
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
        node.toString
      stream.println(graph.block)
      stream.println("==================")
      source.toString
    }*/
  }

  test("show") {
    val driver = new CompilerCFusedTensor[Int, Unit] {
      import FusedTensorTypeLess._

      @virtualize
      def snippet(arg: Rep[Int]): Rep[Unit] = {
        val array = NewArray[Int](10)
        // val a = Tensor(6, array, i => 0)
        val a = Tensor.zeros[Int](10, array)
        val b = Tensor.ones[Int](10, array)
        val c = a + b
        // c.show; ()
        printf("%d", c(0))
        // a.show; ()
        // val b = Tensor(6, array, i => 1)

        // val c = Tensor.zeros[Int](10, array)
        // printf("%d", a(1) + b(1))
        //val c = Tensor(6, array, i => a(i) + b(i))
        //c.show; ()
      }
    }
    System.out.println(indent(driver.code))
  }
}