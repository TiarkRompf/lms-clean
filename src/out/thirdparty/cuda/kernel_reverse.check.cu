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
__global__ void x7(int* x8, int x9) {
  __shared__ int x10[64];
  int x11 = threadIdx.x;
  x10[x11] = x8[x11];
  __syncthreads();
  x8[x11] = x10[x9 - x11 - 1];
}
__global__ void x14(int* x15, int x16) {
  extern __shared__ int x17[];
  int x18 = threadIdx.x;
  x17[x18] = x15[x18];
  __syncthreads();
  x15[x18] = x17[x16 - x18 - 1];
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(64 * sizeof(int));
  int* x2 = (int*)malloc(64 * sizeof(int));
  int* x3 = (int*)malloc(64 * sizeof(int));
  int x4 = 0;
  while (x4 != 64) {
    int x5 = x4;
    x1[x5] = x5;
    x2[x5] = 64 - x5 - 1;
    x3[x5] = 0;
    x4 = x4 + 1;
  }
  int* x6 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(64 * sizeof(int))));
  cudaMemcpy(x6, x1, (size_t)(64 * sizeof(int)), cudaMemcpyHostToDevice);
  x7<<<dim3(1, 1, 1), dim3(64, 1, 1)>>>(x6, 64);
  cudaMemcpy(x3, x6, (size_t)(64 * sizeof(int)), cudaMemcpyDeviceToHost);
  int x12 = 0;
  while (x12 != 64) {
    int x13 = x12;
    if (x3[x13] != x2[x13]) printf("Error!");
    x12 = x12 + 1;
  }
  cudaMemcpy(x6, x1, (size_t)(64 * sizeof(int)), cudaMemcpyHostToDevice);
  x14<<<dim3(1, 1, 1), dim3(64, 1, 1), dim1(1)>>>(x6, 64);
  cudaMemcpy(x3, x6, (size_t)(64 * sizeof(int)), cudaMemcpyDeviceToHost);
  int x19 = 0;
  while (x19 != 64) {
    int x20 = x19;
    if (x3[x20] != x2[x20]) printf("Error!");
    x19 = x19 + 1;
  }
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
