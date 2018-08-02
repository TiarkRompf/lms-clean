// Initial code:
def tensors_01(x0: Int): Int = {
  val x1 = tensor_ones(List(3, 4, 5))
  val x2 = tensor_add(x1, x1)
  println(x2)
  println(tensor_add(x2, x1))
  0
}
// After Tensor lowering:
def tensors_01(x0: Int): Int = {
  val x1 = tensor(List(3, 4, 5), (x2 => 1))
  val x3 = tensor(List(3, 4, 5), { x5 =>
    val x4 = tensor_apply(x1, x5)
    x4 + x4
  })
  println(x3)
  println(tensor(List(3, 4, 5), (x6 => tensor_apply(x3, x6) + tensor_apply(x1, x6))))
  0
}
// After Tensor fusion V:
def tensors_01(x0: Int): Int = {
  println(tensor(List(3, 4, 5), (x1 => 2)))
  println(tensor(List(3, 4, 5), (x2 => 3)))
  0
}
// After Tensor fusion H:
def tensors_01(x0: Int): Int = {
  val x1 = fused-tensor(List(3, 4, 5), (x2 => List(2, 3)))
  println(seq-apply(x1, 0))
  println(seq-apply(x1, 1))
  0
}
