/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x2(int x3, int x4, int x5) {
  int x6 = gridDim.x * blockDim.x;
  int x7 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x7 < x5) {
    x3[x7] = x3[x7];
    x7 = x7 + x6;
  }
}
__global__ void x8(int x9, int x10, int x11) {
  int x12 = gridDim.x * blockDim.x;
  int x13 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x13 < x11) {
    x9[x13] = x9[x13] - 1;
    x13 = x13 + x12;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x1, (size_t)(10 * sizeof(int))));
  show_tensor(x2<<<dim3(0, 1, 1), dim3(0, 1, 1)>>>(x1, 0, 10));
  show_tensor(x8<<<dim3(0, 1, 1), dim3(0, 1, 1)>>>(x1, 0, 10));
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
