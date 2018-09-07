// Initial code:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(3, 4, 5), ((x3: Int) /* x4 */ => seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2) /* x4 */))
  val x5 = tensor_add1(x2, x2)
  println(x5)/* val x6 = "CTRL":x1 */
  println(tensor_add1(x5, x2))/* val x7 = "CTRL":x6 */
  0 /* x7 */
}
// After Tensor lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = Tensor1(List(3, 4, 5), ((x3: Int) /* x4 */ => seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2) /* x4 */))
  val x5 = Tensor1(List(3, 4, 5), ({ x7: Int /* x8 */ =>
    val x6 = tensor_apply(x2, x7)
    x6 + x6 /* x8 */
  }))
  println(x5)/* val x9 = "CTRL":x1 */
  println(Tensor1(List(3, 4, 5), ((x10: Int) /* x11 */ => tensor_apply(x5, x10) + tensor_apply(x2, x10) /* x11 */)))/* val x12 = "CTRL":x9 */
  0 /* x12 */
}
// After Tensor fusion V:
def tensors_05(x0: Int): Int /* x1 */ = {
  println(Tensor1(List(3, 4, 5), ({ x3: Int /* x4 */ =>
    val x2 = seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2)
    x2 + x2 /* x4 */
  })))/* val x5 = "CTRL":x1 */
  println(Tensor1(List(3, 4, 5), ({ x7: Int /* x8 */ =>
    val x6 = seq_apply(x7, 0) + seq_apply(x7, 1) + seq_apply(x7, 2)
    x6 + x6 + x6 /* x8 */
  })))/* val x9 = "CTRL":x5 */
  0 /* x9 */
}
// After Tensor fusion H:
def tensors_05(x0: Int): Int /* x1 */ = {
  println(Tensor1(List(3, 4, 5), ({ x3: Int /* x4 */ =>
    val x2 = seq_apply(x3, 0) + seq_apply(x3, 1) + seq_apply(x3, 2)
    x2 + x2 /* x4 */
  })))/* val x5 = "CTRL":x1 */
  println(Tensor1(List(3, 4, 5), ({ x7: Int /* x8 */ =>
    val x6 = seq_apply(x7, 0) + seq_apply(x7, 1) + seq_apply(x7, 2)
    x6 + x6 + x6 /* x8 */
  })))/* val x9 = "CTRL":x5 */
  0 /* x9 */
}
// After Multiloop/Builder lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  val x2 = TensorBuilder1(List(3, 4, 5))/* val x2 = "STORE":x1 */
  val x3 = forloops(List(3, 4, 5), ({ x5: Int /* x7 */ =>
    val x4 = seq_apply(x5, 0) + seq_apply(x5, 1) + seq_apply(x5, 2)
    val x6 = builder_add(x2, x5, x4 + x4)/* val x6 = "STORE":x7 */
    /* x6 */
  }))/* val x3 = "STORE":x2 */
  println(builder_res(x2)/* val x8 = "STORE":x3 */)/* val x9 = "CTRL":x1 */
  val x10 = TensorBuilder1(List(3, 4, 5))/* val x10 = "STORE":x1x8 */
  val x11 = forloops(List(3, 4, 5), ({ x13: Int /* x15 */ =>
    val x12 = seq_apply(x13, 0) + seq_apply(x13, 1) + seq_apply(x13, 2)
    val x14 = builder_add(x10, x13, x12 + x12 + x12)/* val x14 = "STORE":x15 */
    /* x14 */
  }))/* val x11 = "STORE":x10x1 */
  println(builder_res(x10)/* val x16 = "STORE":x11x1 */)/* val x17 = "CTRL":x1x9 */
  0 /* x17x16 */
}
x22 = (forloops Const(List(3, 4, 5)) x21 Eff(List(x11))) // Set(x0)
x47 = (forloops Const(List(3, 4, 5)) x46 Eff(List(x35, x0))) // Set(x0)
stms:
x11 = (TensorBuilder1 Const(List(3, 4, 5)) Eff(List(x0)))
x21 = (λ Block(List(x13, x12),Const(()),List(x20)))
x22 = (forloops Const(List(3, 4, 5)) x21 Eff(List(x11)))
x23 = (builder_res x11 Eff(List(x22)))
x24 = (P x23 Eff(List(x0)))
x35 = (TensorBuilder1 Const(List(3, 4, 5)) Eff(List(x0, x23)))
x46 = (λ Block(List(x37, x36),Const(()),List(x45)))
x47 = (forloops Const(List(3, 4, 5)) x46 Eff(List(x35, x0)))
x48 = (builder_res x35 Eff(List(x47, x0)))
x49 = (P x48 Eff(List(x0, x24)))
List(List())
List(List(x35 = (TensorBuilder1 Const(List(3, 4, 5)) Eff(List(x0, x23)))))
List(List(x11 = (TensorBuilder1 Const(List(3, 4, 5)) Eff(List(x0)))))
List(List(x14 = (seq_apply x13 Const(0))))
List(List(x15 = (seq_apply x13 Const(1))))
List(List(x16 = (+ x14 x15)))
List(List(x17 = (seq_apply x13 Const(2))))
List(List(x18 = (+ x16 x17)))
List(List(x19 = (+ x18 x18)))
List(List(x20 = (builder_add x11 x13 x19 Eff(List(x12)))))
List(List(x21 = (λ Block(List(x13, x12),Const(()),List(x20)))))
List(List(x38 = (seq_apply x37 Const(0))))
List(List(x39 = (seq_apply x37 Const(1))))
List(List(x40 = (+ x38 x39)))
List(List(x41 = (seq_apply x37 Const(2))))
List(List(x42 = (+ x40 x41)))
List(List(x43 = (+ x42 x42)))
List(List(x44 = (+ x43 x42)))
List(List(x45 = (builder_add x35 x37 x44 Eff(List(x36)))))
List(List(x46 = (λ Block(List(x37, x36),Const(()),List(x45)))))
List(List(x22 = (forloops Const(List(3, 4, 5)) x21 Eff(List(x11))), x47 = (forloops Const(List(3, 4, 5)) x46 Eff(List(x35, x0)))))
List(List(x48 = (builder_res x35 Eff(List(x47, x0)))))
List(List(x23 = (builder_res x11 Eff(List(x22)))))
List(List(x24 = (P x23 Eff(List(x0)))))
List(List(x49 = (P x48 Eff(List(x0, x24)))))
// After Tensor fusion H2:
def tensors_05(x0: Int): Int /* x1 */ = {
  0 /* x1 */
}
// After MultiDim foreach lowering:
def tensors_05(x0: Int): Int /* x1 */ = {
  0 /* x1 */
}
