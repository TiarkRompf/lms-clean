package lms.transformation.util

import scala.collection.immutable._

trait DataStructure {

  /**
   * This is the UnionFind data structure.
   *
   * One use case it to give Tensor Dimensions names (named dimension), which have to be unified based on operation features.
   * In AIRCoP, the dimensions are named. However, if we don't expect the user to provide the names correctly,
   * we have to unify the names based on operation features. Then we would need a union find data structure.
   */
  class USet[T](val ele: T, var parent: USet[T], var size: Int = 1) {

    def head: USet[T] = if (parent == this) this else {
      val temp = parent.head
      parent = temp
      temp
    }

    def merge(that: USet[T]): Unit = {
      val this_head = head
      val that_head = that.head
      if (this_head != that_head && this_head.size < that_head.size) {
        mergeInto(this_head, that_head)
      } else if (this_head != that_head) {
        mergeInto(that_head, this_head)
      }
    }

    def mergeInto(a: USet[T], b: USet[T]): Unit = {
      a.parent = b
      b.size += a.size
    }

  }

  class USets[T](val starts: List[T]) {

    val workspace: Map[T, USet[T]] = starts.map(x => {
      val temp = new USet[T](x, null, 1)
      temp.parent = temp
      (x, temp)
    }).toMap

    def merge(a: T, b: T) = workspace(a).merge(workspace(b))

    lazy val union_map: Map[T, T] = workspace.map { case (k, v) =>
      (k, v.head.ele)
    }.toMap

  }
}

// encapsulates parameters to a cudnn convolution operation
trait ConvParam {
  case class ConvParam(val alpha: Float, val beta: Float, val padding: Seq[Int], val strides: Seq[Int], val dilation: Seq[Int])
}
