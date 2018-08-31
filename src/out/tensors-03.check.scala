// Initial code:
def tensors_03(x0: Int): Int = {
  println(foobar(7))
  0
}
// After Tensor lowering:
def tensors_03(x0: Int): Int = {
  println(foobar(7))
  0
}
// After Tensor fusion V:
def tensors_03(x0: Int): Int = {
  println(foobar(7))
  0
}
// After Tensor fusion H:
def tensors_03(x0: Int): Int = {
  println(foobar(7))
  0
}
// After Multiloop lowering:
def tensors_03(x0: Int): Int = {
  println(foobar(7))
  0
}
// After Multiloop/Builder lowering:
def tensors_03(x0: Int): Int = {
  println(14)
  0
}
// After MultiDim foreach lowering:
def tensors_03(x0: Int): Int = {
  println(14)
  0
}
