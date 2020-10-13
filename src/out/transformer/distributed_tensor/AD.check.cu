/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  module([&]() {
    float x1 = tensor_weight(TensorType(List(Size(Dim(0),3), Size(Dim(1),3)),Float,NAnno), NAnno);
    float x2 = tensor_weight(TensorType(List(Size(Dim(0),3), Size(Dim(1),3)),Float,NAnno), NAnno);
    float x3 = tensor_weight(TensorType(List(Size(Dim(0),3), Size(Dim(1),3)),Float,NAnno), NAnno);
    int x4 = 0;
    while (x4 != 5) {
      float x5 = tensor_input(TensorType(List(Size(Dim(0),3), Size(Dim(1),3)),Float,NAnno), NAnno);
      float x6 = tensor_ones(TensorType(List(Size(Dim(0),3), Size(Dim(1),3)),Float,NAnno), NAnno);
      float x7 = tensor_mult(TensorType(List(Size(Dim(0),3), Size(Dim(1),3)),Float,NAnno), NAnno, x5, x6);
      accum_tensor(NAnno, x2, x7);
      optimize_tensor(NAnno, x1, x2, x3);
      x4 = x4 + 1;
    }
    save(x1);
  })(());
  printf("compile");
}
/*****************************************
End of C Generated Code
*******************************************/
int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("usage: %s <arg>\n", argv[0]);
    return 0;
  }
  Snippet(atoi(argv[1]));
  return 0;
}
