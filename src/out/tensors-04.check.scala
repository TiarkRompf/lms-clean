// Initial code:
def tensors_04(x0: Int): Int = {
  println(Tensor1(List(3, 4, 5), ((x1: Int) => seq_apply(x1, 0) + seq_apply(x1, 1) + seq_apply(x1, 2))))
  0
}
// After Tensor lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), ({ x4: Int =>
    builder_add(x1, x4, seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2))
  }))
  println(builder_res(x1))
  0
}
// After Tensor fusion V:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), ({ x4: Int =>
    builder_add(x1, x4, seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2))
  }))
  println(builder_res(x1))
  0
}
// After Tensor fusion H:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), ({ x4: Int =>
    builder_add(x1, x4, seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2))
  }))
  println(builder_res(x1))
  0
}
// After Multiloop lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), ({ x4: Int =>
    builder_add(x1, x4, seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2))
  }))
  println(builder_res(x1))
  0
}
// After Multiloop/Builder lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), ({ x4: Int =>
    builder_add(x1, x4, seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2))
  }))
  println(builder_res(x1))
  0
}
// After MultiDim foreach lowering:
def tensors_04(x0: Int): Int = {
  val x1 = new Array[Int](60)
  forloop(3, ({ x4: Int =>
    val x3 = x4 * 20
    forloop(4, ({ x7: Int =>
      val x6 = x4 + x7
      val x8 = x7 * 5
      forloop(5, ({ x10: Int =>
        x1(x3 + x8 + x10) = x6 + x10
      }))
    }))
  }))
  println(x1)
  0
}
