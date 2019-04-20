// Initial code:
def tensors_06(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(100), (x3: Int) /* x4 */ => 1 /* x4 */)
  val x5 = Tensor1(List(100), (x6: Int) /* x7 */ => 2 * seq_apply(x6, 0) /* x7 */)
  val x8 = Tensor1(List(100), (x9: Int) /* x10 */ => tensor_apply(x2, x9) + tensor_apply(x5, x9) /* x10 */)
  val x11 = Sum(List(100), (x12: Int) /* x13 */ => tensor_apply(x8, x12) /* x13 */) / 100
  println(Sum(List(100), (x14: Int) /* x15 */ => tensor_apply(x8, x14) /* x15 */) / 100)/* val x16 = "CTRL":x1 */
  println(Sum(List(100), { (x17: Int) /* x18 */ =>
    val x19 = tensor_apply(x8, x17)
    x19 * x19 /* x18 */
  }) / 100 - x11 * x11)/* val x20 = "CTRL":x16 */
  0 /* x20 */
}
// After Tensor lowering:
def tensors_06(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(100), (x3: Int) /* x4 */ => 1 /* x4 */)
  val x5 = Tensor1(List(100), (x6: Int) /* x7 */ => 2 * seq_apply(x6, 0) /* x7 */)
  val x8 = Tensor1(List(100), (x9: Int) /* x10 */ => tensor_apply(x2, x9) + tensor_apply(x5, x9) /* x10 */)
  val x11 = Sum(List(100), (x12: Int) /* x13 */ => tensor_apply(x8, x12) /* x13 */) / 100
  println(Sum(List(100), (x14: Int) /* x15 */ => tensor_apply(x8, x14) /* x15 */) / 100)/* val x16 = "CTRL":x1 */
  println(Sum(List(100), { (x17: Int) /* x18 */ =>
    val x19 = tensor_apply(x8, x17)
    x19 * x19 /* x18 */
  }) / 100 - x11 * x11)/* val x20 = "CTRL":x16 */
  0 /* x20 */
}
// After Tensor fusion V:
def tensors_06(x0: Int): Int /* x1 */ = {
  val x2 = Sum(List(100), (x3: Int) /* x4 */ => 1 + 2 * seq_apply(x3, 0) /* x4 */) / 100
  println(Sum(List(100), (x5: Int) /* x6 */ => 1 + 2 * seq_apply(x5, 0) /* x6 */) / 100)/* val x7 = "CTRL":x1 */
  println(Sum(List(100), { (x8: Int) /* x9 */ =>
    val x10 = 1 + 2 * seq_apply(x8, 0)
    x10 * x10 /* x9 */
  }) / 100 - x2 * x2)/* val x11 = "CTRL":x7 */
  0 /* x11 */
}
// After Tensor fusion H:
def tensors_06(x0: Int): Int /* x1 */ = {
  val x2 = Sum(List(100), (x3: Int) /* x4 */ => 1 + 2 * seq_apply(x3, 0) /* x4 */) / 100
  println(Sum(List(100), (x5: Int) /* x6 */ => 1 + 2 * seq_apply(x5, 0) /* x6 */) / 100)/* val x7 = "CTRL":x1 */
  println(Sum(List(100), { (x8: Int) /* x9 */ =>
    val x10 = 1 + 2 * seq_apply(x8, 0)
    x10 * x10 /* x9 */
  }) / 100 - x2 * x2)/* val x11 = "CTRL":x7 */
  0 /* x11 */
}
// After Multiloop/Builder lowering:
def tensors_06(x0: Int): Int /* x1 */ = {
  val x2 = SumBuilder1(0)/* val x2 = Const(STORE):x1 */
  val x3 = forloops(List(100), { (x4: Int) /* x5 */ =>
    val x6 = sum_builder_add(x2, x4, 1 + 2 * seq_apply(x4, 0))/* val x6 = x2:x5 */
    /* x6 */
  })/* val x3 = x2:x2 */
  val x7 = SumBuilder1(0)/* val x7 = Const(STORE):x2 */
  val x8 = forloops(List(100), { (x9: Int) /* x10 */ =>
    val x11 = 1 + 2 * seq_apply(x9, 0)
    val x12 = sum_builder_add(x7, x9, x11 * x11)/* val x12 = x7:x10 */
    /* x12 */
  })/* val x8 = x7:x7 */
  val x13 = SumBuilder1(0)/* val x13 = Const(STORE):x7 */
  val x14 = forloops(List(100), { (x15: Int) /* x16 */ =>
    val x17 = sum_builder_add(x13, x15, 1 + 2 * seq_apply(x15, 0))/* val x17 = x13:x16 */
    /* x17 */
  })/* val x14 = x13:x13 */
  val x18 = sum_builder_res(x13)/* val x19 = x13:x14 */ / 100
  println(sum_builder_res(x2)/* val x20 = x2:x3 */ / 100)/* val x21 = "CTRL":x1 */
  println(sum_builder_res(x7)/* val x22 = x7:x8 */ / 100 - x18 * x18)/* val x23 = "CTRL":x21 */
  0 /* x13x20x19x23x22 */
}
// After Tensor fusion H2:
def tensors_06(x0: Int): Int /* x1 */ = {
  val x2 = SumBuilder1(0)/* val x2 = Const(STORE):x1 */
  val x3 = SumBuilder1(0)/* val x3 = Const(STORE):x2 */
  val x4 = SumBuilder1(0)/* val x4 = Const(STORE):x3 */
  val x5 = forloops(List(100), { (x6: Int) /* x7 */ =>
    val x8 = 1 + 2 * seq_apply(x6, 0)
    val x9 = sum_builder_add(x2, x6, x8)/* val x9 = x2:x7 */
    val x10 = sum_builder_add(x3, x6, x8 * x8)/* val x10 = x3:x7 */
    val x11 = sum_builder_add(x4, x6, x8)/* val x11 = x4:x7 */
    /* x9x10x11 */
  })/* val x5 = x2,x3,x4:x2x3x4 */
  val x12 = sum_builder_res(x4)/* val x13 = x4:x5 */ / 100
  println(sum_builder_res(x2)/* val x14 = x2:x5 */ / 100)/* val x15 = "CTRL":x1 */
  println(sum_builder_res(x3)/* val x16 = x3:x5 */ / 100 - x12 * x12)/* val x17 = "CTRL":x15 */
  0 /* x13x4x16x17x14 */
}
// After MultiDim foreach lowering:
def tensors_06(x0: Int): Int /* x1 */ = {
  var x2 = 0
  var x3 = 0
  var x4 = 0
  val x5 = forloop(100, { (x6: Int) /* x7 */ =>
    val x8 = 1 + 2 * x6
    x2 = x2/* x2:x7 */ + x8
    x3 = x3/* x3:x7 */ + x8 * x8
    x4 = x4/* x4:x7 */ + x8
    /* x12x13x14 */
  })/* val x5 = x2,x3,x4:x2x3x4 */
  val x15 = x4/* val x16 = x4:x5 */ / 100
  println(x2/* val x17 = x2:x5 */ / 100)/* val x18 = "CTRL":x1 */
  println(x3/* val x19 = x3:x5 */ / 100 - x15 * x15)/* val x20 = "CTRL":x18 */
  0 /* x19x16x4x20x17 */
}
