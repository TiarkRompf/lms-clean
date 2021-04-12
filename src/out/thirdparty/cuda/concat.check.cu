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
__global__ void x11(int* x12, int* x13, int* x14, int x15, int x16, int x17) {
  // this is cuda concat kernel. It concatenates two 3D arrays and concat on the innermost dimension.
  // in1: first input array
  // in2: second input array
  // out: output array
  // d1: dim0 of the first input array
  // d2: dim0 of the second input array
  int x18 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x18 < x17) {
    int x19 = x15 + x16;
    int x20 = x18 % x19;
    x14[x18] = x20 < x15 ? x12[x18 / x19 * x15 + x20] : x13[x18 / x19 * x16 + (x20 - x15)];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(9 * sizeof(int));
  int x2 = 0;
  while (x2 != 9) {
    int x3 = x2;
    x1[x3] = 100 + x3;
    x2 = x2 + 1;
  }
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(9 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x4, x1, (size_t)(9 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x5 = (int*)malloc(15 * sizeof(int));
  int x6 = 0;
  while (x6 != 15) {
    int x7 = x6;
    x5[x7] = x7;
    x6 = x6 + 1;
  }
  int* x8 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(15 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x8, x5, (size_t)(15 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x9 = (int*)malloc(24 * sizeof(int));
  int* x10 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(24 * sizeof(int))));
  x11<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x4, x8, x10, 3, 5, 24);
  CUDA_CALL(cudaMemcpy(x9, x10, (size_t)(24 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("output: [");
  int x21 = 0;
  while (x21 != 24) {
    printf("%d,", x9[x21]);
    x21 = x21 + 1;
  }
  printf("]\n");
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
