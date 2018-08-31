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
  val x1 = multiloop(List(3, 4, 5), List(tensor, tensor), (x2 => List(2, 3)))
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
    val x4 = x3
    var x5 = 0
    val x6 = x4 * 20
    while (x5 != 4) {
      val x7 = x5
      var x8 = 0
      val x9 = x7 * 5
      while (x8 != 5) {
        val x10 = x8
        val x11 = x6 + x9 + x10
        x1(x11) = 2
        x2(x11) = 3
        x8 = x8 + 1
      }
      x5 = x5 + 1
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
  foreach(List(3, 4, 5), { x5 =>
    tensor_builder_add(x1, x5, 2)
    tensor_builder_add(x2, x5, 3)
  })
  val x7 = tensor_builder_get(x1)
  val x8 = tensor_builder_get(x2)
  println(x7)
  println(x8)
  0
}
// After MultiDim foreach lowering:
def tensors_01(x0: Int): Int = {
  val x1 = arraype_new(60)
  val x2 = arraype_new(60)
  foreach(3, { x5 =>
    val x4 = x5 * 20
    foreach(4, { x8 =>
      val x7 = x8 * 5
      foreach(5, { x11 =>
        val x10 = x4 + x7 + x11
        x1(x10) = 2
        x2(x10) = 3
      })
    })
  })
  println(x1)
  println(x2)
  0
}
