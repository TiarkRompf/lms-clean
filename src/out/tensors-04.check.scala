// Initial code:
def tensors_04(x0: Int): Int = {
  println(Tensor1(7, ((x1: Int) => x1 + 8)))
  0
}
// After Tensor lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(7)
  forloop(7, ({ x4: Int =>
    builder_add(x1, x4, x4 + 8)
  }))
  println(builder_res(x1))
  0
}
// After Tensor fusion V:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(7)
  forloop(7, ({ x4: Int =>
    builder_add(x1, x4, x4 + 8)
  }))
  println(builder_res(x1))
  0
}
// After Tensor fusion H:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(7)
  forloop(7, ({ x4: Int =>
    builder_add(x1, x4, x4 + 8)
  }))
  println(builder_res(x1))
  0
}
// After Multiloop lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(7)
  forloop(7, ({ x4: Int =>
    builder_add(x1, x4, x4 + 8)
  }))
  println(builder_res(x1))
  0
}
// After Multiloop/Builder lowering:
def tensors_04(x0: Int): Int = {
  val x1 = new Array[Int](7)
  forloop(7, ({ x3: Int =>
    x1(x3) = x3 + 8
  }))
  println(x1)
  0
}
// After MultiDim foreach lowering:
def tensors_04(x0: Int): Int = {
  val x1 = new Array[Int](7)
  var x2 = 0
  while (x2 != 7) {
    val x3 = x2
    x1(x3) = x3 + 8
    x2 = x2 + 1
  }
  println(x1)
  0
}
