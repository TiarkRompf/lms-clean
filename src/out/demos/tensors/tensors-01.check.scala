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
  val x1 = tensor(List(3, 4, 5), x2 => 1)
  val x3 = tensor(List(3, 4, 5), { x4 =>
    val x5 = tensor_apply(x1, x4)
    x5 + x5
  })
  println(x3)
  println(tensor(List(3, 4, 5), x6 => tensor_apply(x3, x6) + tensor_apply(x1, x6)))
  0
}
// After Tensor fusion V:
def tensors_01(x0: Int): Int = {
  println(tensor(List(3, 4, 5), x1 => 2))
  println(tensor(List(3, 4, 5), x2 => 3))
  0
}
// After Tensor fusion H:
def tensors_01(x0: Int): Int = {
  val x1 = multiloop(List(3, 4, 5), List(tensor, tensor), x2 => List(2, 3))
  println(seq_apply(x1, 0))
  println(seq_apply(x1, 1))
  0
}
// After Multiloop lowering:
def tensors_01(x0: Int): Int = {
  val x1 = new Array[Int](60)
  val x2 = new Array[Int](60)
  var x3 = 0
  while (x3 != 3) {
    var x4 = 0
    val x5 = x3 * 20
    while (x4 != 4) {
      var x6 = 0
      val x7 = x4 * 5
      while (x6 != 5) {
        val x8 = x5 + x7 + x6
        x1(x8) = 2
        x2(x8) = 3
        x6 = x6 + 1
      }
      x4 = x4 + 1
    }
    x3 = x3 + 1
  }
  println(x1)
  println(x2)
  0
}
// After Multiloop/Builder lowering:
def tensors_01(x0: Int): Int = {
  val x1 = tensor_builder_new(List(3, 4, 5))
  val x2 = tensor_builder_new(List(3, 4, 5))
  foreach(List(3, 4, 5), { x4 =>
    tensor_builder_add(x1, x4, 2)
    tensor_builder_add(x2, x4, 3)
  })
  println(tensor_builder_get(x1))
  println(tensor_builder_get(x2))
  0
}
// After MultiDim foreach lowering:
def tensors_01(x0: Int): Int = {
  val x1 = new Array[Int](60)
  val x2 = new Array[Int](60)
  foreach(3, { x4 =>
    val x5 = x4 * 20
    foreach(4, { x7 =>
      val x8 = x7 * 5
      foreach(5, { x10 =>
        val x11 = x5 + x8 + x10
        x1(x11) = 2
        x2(x11) = 3
      })
    })
  })
  println(x1)
  println(x2)
  0
}
