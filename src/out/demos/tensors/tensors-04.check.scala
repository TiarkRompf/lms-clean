// Initial code:
def tensors_04(x0: Int): Int = {
  val x1 = Tensor1(List(3, 4, 5), (x2: Int) => seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2))
  println(tensor_add1(x1, x1))
  0
}
// After Tensor lowering:
def tensors_04(x0: Int): Int = {
  val x1 = Tensor1(List(3, 4, 5), (x2: Int) => seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2))
  println(Tensor1(List(3, 4, 5), { (x3: Int) =>
    val x4 = tensor_apply(x1, x3)
    x4 + x4
  }))
  0
}
// After Tensor fusion V:
def tensors_04(x0: Int): Int = {
  println(Tensor1(List(3, 4, 5), { (x1: Int) =>
    val x2 = seq_apply(x1, 0) + seq_apply(x1, 1) + seq_apply(x1, 2)
    x2 + x2
  }))
  0
}
// After Tensor fusion H:
def tensors_04(x0: Int): Int = {
  println(Tensor1(List(3, 4, 5), { (x1: Int) =>
    val x2 = seq_apply(x1, 0) + seq_apply(x1, 1) + seq_apply(x1, 2)
    x2 + x2
  }))
  0
}
// After Multiloop/Builder lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), { (x3: Int) =>
    val x4 = seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2)
    builder_add(x1, x3, x4 + x4)
  })
  println(builder_res(x1))
  0
}
// After Tensor fusion H2:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), { (x3: Int) =>
    val x4 = seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2)
    builder_add(x1, x3, x4 + x4)
  })
  println(builder_res(x1))
  0
}
// After MultiDim foreach lowering:
def tensors_04(x0: Int): Int = {
  val x1 = new Array[Int](60)
  forloop(3, { (x3: Int) =>
    val x4 = x3 * 20
    forloop(4, { (x6: Int) =>
      val x7 = x3 + x6
      val x8 = x6 * 5
      forloop(5, { (x10: Int) =>
        val x11 = x7 + x10
        x1(x4 + (x8 + x10)) = x11 + x11
      })
    })
  })
  println(x1)
  0
}
