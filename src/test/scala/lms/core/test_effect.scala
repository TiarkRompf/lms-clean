package lms
package core

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.collection.immutable._

@virtualize
class EffectTest extends TutorialFunSuite {
  val under = "effect/"

  /* Test cases adapted from Nada Amin's issue: https://github.com/TiarkRompf/lms-clean/issues/122 */

  test("array_update_ret_1") {
    val snippet = new DslDriver[Array[Int],Array[Int]] {
      def snippet(a: Rep[Array[Int]]) = {
        a(0) = 0
        a
      }
    }
    check("array_update_ret_1", snippet.code, "scala")
  }

  test("array_update_ret_2") {
    val snippet = new DslDriver[(Array[Int],Int),Array[Int]] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(t: Rep[(Array[Int],Int)]) = {
        val a = t._1
        a(0) = 0
        a
      }
    }
    check("array_update_ret_2", snippet.code, "scala")
  }

  test("array_update_ret_3") {
    val snippet = new DslDriver[Array[Int],(Array[Int],Int)] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(a: Rep[Array[Int]]) = {
        a(0) = 0
        (a, 0)
      }
    }
    check("array_update_ret_3", snippet.code, "scala")
  }

  test("array_update_ret_4") {
    val snippet = new DslDriver[(Array[Int],Int),(Array[Int],Int)] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(t: Rep[(Array[Int],Int)]) = {
        val a = t._1
        a(0) = 0
        (a, 0)
      }
    }
    check("array_update_ret_4", snippet.code, "scala")
  }

  test("array_update_ret_5") {
    val snippet = new DslDriver[(Array[Int],Int),(Array[Int],Int)] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(t: Rep[(Array[Int],Int)]) = {
        val a = t._1
        val b = t._2
        a(0) = 0
        (a, b)
      }
    }
    check("array_update_ret_5", snippet.code, "scala")
  }

  test("array_update_ret_6") {
    val snippet = new DslDriver[(Array[Int],Int),(Array[Int],Int)] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(t: Rep[(Array[Int],Int)]) = {
        val a = t._1
        val b = t._2
        a(0) = 0
        val (oa, ob) = (a, b)
        (oa, ob)
      }
    }
    check("array_update_ret_6", snippet.code, "scala")
  }

  test("array_update_ret_7") {
    val snippet = new DslDriver[(Array[Int],Int),(Array[Int],Int)] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(t: Rep[(Array[Int],Int)]) = {
        val a = t._1
        val b = t._2
        a(0) = 0
        val (oa, ob) = (a, b+1)
        (oa, ob)
      }
    }
    check("array_update_ret_7", snippet.code, "scala")
  }

  test("array_update_ret_8") {
    val snippet = new DslDriver[(Array[Int],Int),(Array[Int],Int)] with TupleOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_Tuple {
        val IR: q.type = q
      }
      def snippet(t: Rep[(Array[Int],Int)]) = {
        val a = t._1
        val b = t._2
        a(0) = 0
        t
      }
    }
    check("array_update_ret_8", snippet.code, "scala")
  }

  test("array_update_ret_9") {
    val snippet = new DslDriver[List[Array[Int]],Array[Int]] with ListOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_List {
        val IR: q.type = q
      }
      def snippet(lst: Rep[List[Array[Int]]]) = {
        val a = lst(0)
        a(0) = 0
        a
      }
    }
    check("array_update_ret_9", snippet.code, "scala")
  }

  test("array_update_ret_10") {
    val snippet = new DslDriver[List[Array[Int]],List[Array[Int]]] with ListOps { q =>
      override val codegen = new DslGen with ScalaCodeGen_List {
        val IR: q.type = q
      }
      def snippet(lst: Rep[List[Array[Int]]]) = {
        val a = lst(0)
        a(0) = 0
        lst
      }
    }
    check("array_update_ret_10", snippet.code, "scala")
  }

  test("array_update_ret_11") {
    val snippet = new DslDriver[Array[Array[Int]],Array[Int]] { q =>
      override val codegen = new DslGen {
        val IR: q.type = q
      }
      def snippet(arr: Rep[Array[Array[Int]]]) = {
        val a = arr(0)
        a(0) = 0
        a
      }
    }
    check("array_update_ret_11", snippet.code, "scala")
  }

  test("array_update_ret_12") {
    val snippet = new DslDriver[Array[Array[Int]],Array[Array[Int]]] { q =>
      override val codegen = new DslGen {
        val IR: q.type = q
      }
      def snippet(arr: Rep[Array[Array[Int]]]) = {
        val a = arr(0)
        a(0) = 0
        arr
      }
    }
    check("array_update_ret_12", snippet.code, "scala")
  }

}
