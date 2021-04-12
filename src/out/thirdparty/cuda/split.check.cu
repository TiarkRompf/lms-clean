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
__global__ void x9(int* x10, int* x11, int* x12, int x13, int x14, int x15) {
  int x16 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x16 < x15) {
    int x17 = x16 % x13;
    if (x17 < x14) x11[x16 / x13 * x14 + x17] = x10[x16];
    else x12[x16 / x13 * (x13 - x14) + (x17 - x14)] = x10[x16];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(16 * sizeof(int));
  int x2 = 0;
  while (x2 != 16) {
    int x3 = x2;
    x1[x3] = x3;
    x2 = x2 + 1;
  }
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(16 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x4, x1, (size_t)(16 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x5 = (int*)malloc(8 * sizeof(int));
  int* x6 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(8 * sizeof(int))));
  int* x7 = (int*)malloc(8 * sizeof(int));
  int* x8 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(8 * sizeof(int))));
  x9<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x4, x6, x8, 4, 2, 16);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(int)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(8 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("output1: [");
  int x18 = 0;
  while (x18 != 8) {
    printf("%d,", x5[x18]);
    x18 = x18 + 1;
  }
  printf("]\n");
  printf("output2: [");
  int x19 = 0;
  while (x19 != 8) {
    printf("%d,", x7[x19]);
    x19 = x19 + 1;
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
