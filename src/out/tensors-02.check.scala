// Initial code:
def tensors_02(x0: Int): Int = {
  val x1 = tensor(List(100), (x2 => 1))
  val x3 = tensor(List(100), (x4 => 2 * seq_apply(x4, 0)))
  val x5 = tensor(List(100), (x6 => tensor_apply(x1, x6) + tensor_apply(x3, x6)))
  val x7 = sum(List(100), (x8 => tensor_apply(x5, x8))) / 100
  println(sum(List(100), (x9 => tensor_apply(x5, x9))) / 100)
  println(sum(List(100), { x11 =>
    val x10 = tensor_apply(x5, x11)
    x10 * x10
  }) / 100 - x7 * x7)
  0
}
// After Tensor lowering:
def tensors_02(x0: Int): Int = {
  val x1 = tensor(List(100), (x2 => 1))
  val x3 = tensor(List(100), (x4 => 2 * seq_apply(x4, 0)))
  val x5 = tensor(List(100), (x6 => tensor_apply(x1, x6) + tensor_apply(x3, x6)))
  val x7 = sum(List(100), (x8 => tensor_apply(x5, x8))) / 100
  println(sum(List(100), (x9 => tensor_apply(x5, x9))) / 100)
  println(sum(List(100), { x11 =>
    val x10 = tensor_apply(x5, x11)
    x10 * x10
  }) / 100 - x7 * x7)
  0
}
// After Tensor fusion V:
def tensors_02(x0: Int): Int = {
  val x1 = sum(List(100), (x2 => 1 + 2 * seq_apply(x2, 0))) / 100
  println(sum(List(100), (x3 => 1 + 2 * seq_apply(x3, 0))) / 100)
  println(sum(List(100), { x5 =>
    val x4 = 1 + 2 * seq_apply(x5, 0)
    x4 * x4
  }) / 100 - x1 * x1)
  0
}
// After Tensor fusion H:
def tensors_02(x0: Int): Int = {
  val x1 = multiloop(List(100), List(sum, sum), { x3 =>
    val x2 = 1 + 2 * seq_apply(x3, 0)
    seq(x2, x2 * x2)
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
    ()
  }
  val x7 = x1
  val x8 = x2
  val x9 = x7 / 100
  println(x9)
  println(x8 / 100 - x9 * x9)
  0
}
