// Initial code:
def tensors_02(x0: Int): Int = {
  val x1 = tensor(List(100), x2 => 1)
  val x3 = tensor(List(100), x4 => 2 * seq_apply(x4, 0))
  val x5 = tensor(List(100), x6 => tensor_apply(x1, x6) + tensor_apply(x3, x6))
  val x7 = sum(List(100), x8 => tensor_apply(x5, x8)) / 100
  println(sum(List(100), x9 => tensor_apply(x5, x9)) / 100)
  println(sum(List(100), { x10 =>
    val x11 = tensor_apply(x5, x10)
    x11 * x11
  }) / 100 - x7 * x7)
  0
}
// After Tensor lowering:
def tensors_02(x0: Int): Int = {
  val x1 = tensor(List(100), x2 => 1)
  val x3 = tensor(List(100), x4 => 2 * seq_apply(x4, 0))
  val x5 = tensor(List(100), x6 => tensor_apply(x1, x6) + tensor_apply(x3, x6))
  val x7 = sum(List(100), x8 => tensor_apply(x5, x8)) / 100
  println(sum(List(100), x9 => tensor_apply(x5, x9)) / 100)
  println(sum(List(100), { x10 =>
    val x11 = tensor_apply(x5, x10)
    x11 * x11
  }) / 100 - x7 * x7)
  0
}
// After Tensor fusion V:
def tensors_02(x0: Int): Int = {
  val x1 = sum(List(100), x2 => 1 + 2 * seq_apply(x2, 0)) / 100
  println(sum(List(100), x3 => 1 + 2 * seq_apply(x3, 0)) / 100)
  println(sum(List(100), { x4 =>
    val x5 = 1 + 2 * seq_apply(x4, 0)
    x5 * x5
  }) / 100 - x1 * x1)
  0
}
// After Tensor fusion H:
def tensors_02(x0: Int): Int = {
  val x1 = multiloop(List(100), List(sum, sum), { x2 =>
    val x3 = 1 + 2 * seq_apply(x2, 0)
    seq(x3, x3 * x3)
  })
  val x4 = seq_apply(x1, 0) / 100
  println(x4)
  println(seq_apply(x1, 1) / 100 - x4 * x4)
  0
}
// After Multiloop lowering:
def tensors_02(x0: Int): Int = {
  var x1 = 0
  var x2 = 0
  var x3 = 0
  while (x3 != 100) {
    val x4 = x3
    val x5 = 1 + 2 * x4
    val x6 = x5 * x5
    x1 = x1 + x5
    x2 = x2 + x6
    x3 = x3 + 1
  }
  val x7 = x1
  val x8 = x2
  val x9 = x7 / 100
  println(x9)
  println(x8 / 100 - x9 * x9)
  0
}
// After Multiloop/Builder lowering:
def tensors_02(x0: Int): Int = {
  val x1 = sum_builder_new(List(100))
  val x2 = sum_builder_new(List(100))
  foreach(List(100), { x4 =>
    val x5 = 1 + 2 * seq_apply(x4, 0)
    val x6 = x5 * x5
    sum_builder_add(x1, x4, x5)
    sum_builder_add(x2, x4, x6)
  })
  val x9 = sum_builder_get(x1)
  val x10 = sum_builder_get(x2)
  val x11 = x9 / 100
  println(x11)
  println(x10 / 100 - x11 * x11)
  0
}
// After MultiDim foreach lowering:
def tensors_02(x0: Int): Int = {
  val x1 = sum_builder_new(List(100))
  val x2 = sum_builder_new(List(100))
  foreach(100, { x4 =>
    val x5 = seq(x4)
    val x6 = 1 + 2 * x4
    sum_builder_add(x1, x5, x6)
    sum_builder_add(x2, x5, x6 * x6)
  })
  val x9 = sum_builder_get(x1) / 100
  println(x9)
  println(sum_builder_get(x2) / 100 - x9 * x9)
  0
}
