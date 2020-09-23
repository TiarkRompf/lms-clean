package lms.transformation.util

import scala.collection.immutable._

trait DataStructure {

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
