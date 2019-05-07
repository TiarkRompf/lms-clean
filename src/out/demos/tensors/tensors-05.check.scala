// Initial code:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(3, 4, 5), (x3: Int) /* x4 */ => seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2) /* x4 */)
  val x5 = tensor_add1(x2, x2)
  println(x5)/* val x6 = [x5 "CTRL"*: _, x1 x5, _] */
  println(tensor_add1(x5, x2))/* val x7 = [x8 "CTRL"*: _, x6 x8, _] */
  0 /* x7 */
}
// After Tensor lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(3, 4, 5), (x3: Int) /* x4 */ => seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2) /* x4 */)
  val x5 = Tensor1(List(3, 4, 5), { (x6: Int) /* x7 */ =>
    val x8 = tensor_apply(x2, x6)
    x8 + x8 /* x7 */
  })
  println(x5)/* val x9 = [x5 "CTRL"*: _, x1 x5, _] */
  println(Tensor1(List(3, 4, 5), (x10: Int) /* x11 */ => tensor_apply(x5, x10) + tensor_apply(x2, x10) /* x11 */))/* val x12 = [x13 "CTRL"*: _, x9 x13, _] */
  0 /* x12 */
}
// After Tensor fusion V:
def tensors_05(x0: Int): Int /* x1 */ = {
  println(Tensor1(List(3, 4, 5), { (x2: Int) /* x3 */ =>
    val x4 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x4 + x4 /* x3 */
  }))/* val x5 = [x6 "CTRL"*: _, x1 x6, _] */
  println(Tensor1(List(3, 4, 5), { (x7: Int) /* x8 */ =>
    val x9 = seq_apply(x7, 0) + seq_apply(x7, 1) + seq_apply(x7, 2)
    x9 + x9 + x9 /* x8 */
  }))/* val x10 = [x11 "CTRL"*: _, x5 x11, _] */
  0 /* x10 */
}
// After Tensor fusion H:
def tensors_05(x0: Int): Int /* x1 */ = {
  println(Tensor1(List(3, 4, 5), { (x2: Int) /* x3 */ =>
    val x4 = seq_apply(x2, 0) + seq_apply(x2, 1) + seq_apply(x2, 2)
    x4 + x4 /* x3 */
  }))/* val x5 = [x6 "CTRL"*: _, x1 x6, _] */
  println(Tensor1(List(3, 4, 5), { (x7: Int) /* x8 */ =>
    val x9 = seq_apply(x7, 0) + seq_apply(x7, 1) + seq_apply(x7, 2)
    x9 + x9 + x9 /* x8 */
  }))/* val x10 = [x11 "CTRL"*: _, x5 x11, _] */
  0 /* x10 */
}
// After Multiloop/Builder lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = TensorBuilder1(List(3, 4, 5))/* val x2 = ["STORE" : _, x1, _] */
  val x3 = forloops(List(3, 4, 5), { (x4: Int) /* x5 */ =>
    val x6 = seq_apply(x4, 0) + seq_apply(x4, 1) + seq_apply(x4, 2)
    val x7 = builder_add(x2, x4, x6 + x6)/* val x7 = [x2*: _, x5, _] */
    /* x5 x7 */
  })/* val x3 = [x2*: _, x2, _] */
  println(builder_res(x2)/* val x8 = [x2*: _, x3, _] */)/* val x9 = [x8 "CTRL"*: _, x1 x8, _] */
  val x10 = TensorBuilder1(List(3, 4, 5))/* val x10 = ["STORE" : _, x1, _] */
  val x11 = forloops(List(3, 4, 5), { (x12: Int) /* x13 */ =>
    val x14 = seq_apply(x12, 0) + seq_apply(x12, 1) + seq_apply(x12, 2)
    val x15 = builder_add(x10, x12, x14 + x14 + x14)/* val x15 = [x10*: _, x13, _] */
    /* x13 x15 */
  })/* val x11 = [x10*: _, x10, _] */
  println(builder_res(x10)/* val x16 = [x10*: _, x11, _] */)/* val x17 = [x16 "CTRL"*: _, x9 x16, _] */
  0 /* x17 */
}
// After Tensor fusion H2:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = TensorBuilder1(List(3, 4, 5))/* val x2 = ["STORE" : _, x1, _] */
  val x3 = TensorBuilder1(List(3, 4, 5))/* val x3 = ["STORE" : _, x1, _] */
  val x4 = forloops(List(3, 4, 5), { (x5: Int) /* x6 */ =>
    val x7 = seq_apply(x5, 0) + seq_apply(x5, 1) + seq_apply(x5, 2)
    val x8 = x7 + x7
    val x9 = builder_add(x3, x5, x8)/* val x9 = [x3*: _, x6, _] */
    val x10 = builder_add(x2, x5, x8 + x7)/* val x10 = [x2*: _, x6, _] */
    /* x6 x9 x10 */
  })/* val x4 = [x2* x3*: _, x2 x3, _] */
  println(builder_res(x3)/* val x11 = [x3*: _, x4, _] */)/* val x12 = [x11 "CTRL"*: _, x1 x11, _] */
  println(builder_res(x2)/* val x13 = [x2*: _, x4, _] */)/* val x14 = [x13 "CTRL"*: _, x12 x13, _] */
  0 /* x14 */
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
        x3(x18) = x17
        x2(x18) = x17 + x16
        /* x15 x19 x20 */
      })/* val x13 = [x2* x3*: _, x10, _] */
      /* x10 x13 */
    })/* val x8 = [x2* x3*: _, x6, _] */
    /* x6 x8 */
  })/* val x4 = [x2* x3*: _, x2 x3, _] */
  println(x3)/* val x21 = [x3 "CTRL"*: _, x1 x4, _] */
  println(x2)/* val x22 = [x2 "CTRL"*: _, x21 x4, _] */
  0 /* x22 */
}
