// Initial code:
def tensors_04(x0: Int): Int = {
  val x1 = Tensor1(List(3, 4, 5), ((x2: Int) => seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)))
  println(Tensor1(List(3, 4, 5), ({ x4: Int =>
    val x3 = tensor_apply(x1, x4)
    x3 + x3
  })))
  0
}
// After Tensor lowering:
def tensors_04(x0: Int): Int = {
  val x1 = Tensor1(List(3, 4, 5), ((x2: Int) => seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)))
  println(Tensor1(List(3, 4, 5), ({ x4: Int =>
    val x3 = tensor_apply(x1, x4)
    x3 + x3
  })))
  0
}
// After Tensor fusion V:
def tensors_04(x0: Int): Int = {
  println(Tensor1(List(3, 4, 5), ({ x2: Int =>
    val x1 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x1 + x1
  })))
  0
}
// After Tensor fusion H:
def tensors_04(x0: Int): Int = {
  println(Tensor1(List(3, 4, 5), ({ x2: Int =>
    val x1 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x1 + x1
  })))
  0
}
// After Multiloop lowering:
def tensors_04(x0: Int): Int = {
  println(Tensor1(List(3, 4, 5), ({ x2: Int =>
    val x1 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x1 + x1
  })))
  0
}
// After Multiloop/Builder lowering:
def tensors_04(x0: Int): Int = {
  val x1 = TensorBuilder1(List(3, 4, 5))
  forloops(List(3, 4, 5), ({ x4: Int =>
    val x3 = seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2)
    builder_add(x1, x4, x3 + x3)
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
      forloop(5, ({ x11: Int =>
        val x10 = x6 + x11
        x1(x3 + x8 + x11) = x10 + x10
      }))
    }))
  }))
  println(x1)
  0
}
