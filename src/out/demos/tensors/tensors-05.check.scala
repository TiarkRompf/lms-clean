// Initial code:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(3, 4, 5), (x3: Int) /* x4 */ => seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2) /* x4 */)
  val x5 = tensor_add1(x2, x2)
  println(x5)/* val x6 = "CTRL":x1 */
  println(tensor_add1(x5, x2))/* val x7 = "CTRL":x6 */
  0 /* x7 */
}
// After Tensor lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(3, 4, 5), (x3: Int) /* x4 */ => seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2) /* x4 */)
  val x5 = Tensor1(List(3, 4, 5), { (x6: Int) /* x7 */ =>
    val x8 = tensor_apply(x2, x6)
    x8 + x8 /* x7 */
  })
  println(x5)/* val x9 = "CTRL":x1 */
  println(Tensor1(List(3, 4, 5), (x10: Int) /* x11 */ => tensor_apply(x5, x10) + tensor_apply(x2, x10) /* x11 */))/* val x12 = "CTRL":x9 */
  0 /* x12 */
}
// After Tensor fusion V:
def tensors_05(x0: Int): Int /* x1 */ = {
  println(Tensor1(List(3, 4, 5), { (x2: Int) /* x3 */ =>
    val x4 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x4 + x4 /* x3 */
  }))/* val x5 = "CTRL":x1 */
  println(Tensor1(List(3, 4, 5), { (x6: Int) /* x7 */ =>
    val x8 = seq_apply(x6, 0) + seq_apply(x6, 1) + seq_apply(x6, 2)
    x8 + x8 + x8 /* x7 */
  }))/* val x9 = "CTRL":x5 */
  0 /* x9 */
}
// After Tensor fusion H:
def tensors_05(x0: Int): Int /* x1 */ = {
  println(Tensor1(List(3, 4, 5), { (x2: Int) /* x3 */ =>
    val x4 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x4 + x4 /* x3 */
  }))/* val x5 = "CTRL":x1 */
  println(Tensor1(List(3, 4, 5), { (x6: Int) /* x7 */ =>
    val x8 = seq_apply(x6, 0) + seq_apply(x6, 1) + seq_apply(x6, 2)
    x8 + x8 + x8 /* x7 */
  }))/* val x9 = "CTRL":x5 */
  0 /* x9 */
}
// After Multiloop/Builder lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = TensorBuilder1(List(3, 4, 5))/* val x2 = Const(STORE):x1 */
  val x3 = forloops(List(3, 4, 5), { (x4: Int) /* x5 */ =>
    val x6 = seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2)
    val x7 = builder_add(x2, x4, x6 + x6)/* val x7 = x2:x5 */
    /* x7 */
  })/* val x3 = x2:x2 */
  println(builder_res(x2)/* val x8 = x2:x3 */)/* val x9 = "CTRL":x1 */
  val x10 = TensorBuilder1(List(3, 4, 5))/* val x10 = Const(STORE):x2 */
  val x11 = forloops(List(3, 4, 5), { (x12: Int) /* x13 */ =>
    val x14 = seq_apply(x12, 0) + seq_apply(x12, 1) + seq_apply(x12, 2)
    val x15 = builder_add(x10, x12, x14 + x14 + x14)/* val x15 = x10:x13 */
    /* x15 */
  })/* val x11 = x10:x10 */
  println(builder_res(x10)/* val x16 = x10:x11 */)/* val x17 = "CTRL":x9 */
  0 /* x10x8x17x16 */
}
// After Tensor fusion H2:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = TensorBuilder1(List(3, 4, 5))/* val x2 = Const(STORE):x1 */
  val x3 = TensorBuilder1(List(3, 4, 5))/* val x3 = Const(STORE):x2 */
  val x4 = forloops(List(3, 4, 5), { (x5: Int) /* x6 */ =>
    val x7 = seq_apply(x5, 0) + seq_apply(x5, 1) + seq_apply(x5, 2)
    val x8 = x7 + x7
    val x9 = builder_add(x2, x5, x8)/* val x9 = x2:x6 */
    val x10 = builder_add(x3, x5, x8 + x7)/* val x10 = x3:x6 */
    /* x9x10 */
  })/* val x4 = x2,x3:x2x3 */
  println(builder_res(x2)/* val x11 = x2:x4 */)/* val x12 = "CTRL":x1 */
  println(builder_res(x3)/* val x13 = x3:x4 */)/* val x14 = "CTRL":x12 */
  0 /* x3x11x13x14 */
}
// After MultiDim foreach lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = new Array[Int](60)
  val x3 = new Array[Int](60)
  val x4 = forloop(3, { (x5: Int) /* x6 */ =>
    val x7 = x5 * 20
    val x8 = forloop(4, { (x9: Int) /* x10 */ =>
      val x11 = x5 + x9
      val x12 = x9 * 5
      val x13 = forloop(5, { (x14: Int) /* x15 */ =>
        val x16 = x11 + x14
        val x17 = x16 + x16
        val x18 = x7 + (x12 + x14)
        x2(x18) = x17
        x3(x18) = x17 + x16
        /* x19x20 */
      })/* val x13 = x2,x3:x10 */
      /* x13 */
    })/* val x8 = x2,x3:x6 */
    /* x8 */
  })/* val x4 = x2,x3:x2x3 */
  println(x2)/* val x21 = "CTRL":x1 */
  println(x3)/* val x22 = "CTRL":x21 */
  0 /* x3x4x22 */
}
