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
__global__ void x5(int* x6, int* x7, int* x8, int x9, int x10, int x11) {
  int x12 = gridDim.x * blockDim.x;
  int x13 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x13 < x11) {
    int x14 = x13;
    if (x6[x14] >= x9 && x6[x14] <= x10) x7[x14] = x7[x14] + x8[x14];
    x13 = x13 + x12;
  }
}
__global__ void x15(int* x16, int* x17, int* x18, int x19, int x20, int x21) {
  int x22 = gridDim.x * blockDim.x;
  int x23 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x23 < x21) {
    int x24 = x23;
    if (x16[x24] < x19 || x16[x24] > x20) x17[x24] = 0;
    x23 = x23 + x22;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1[5] = { 1, 2, 3, 4, 5 };
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(5 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(5 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(5 * sizeof(int))));
  arrayFill<<<28, 512>>>(x3, 8, 5);
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(5 * sizeof(int))));
  x5<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x2, x4, x3, -2, 2, 5);
  printf("%d %d %d %d %d", x4[0], x4[1], x4[2], x4[3], x4[4]);
  x15<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x2, x3, x3, -2, 2, 5);
  printf("%d %d %d %d %d", x3[0], x3[1], x3[2], x3[3], x3[4]);
  CUDA_CALL(cudaFree(x2));
  CUDA_CALL(cudaFree(x3));
  CUDA_CALL(cudaFree(x4));
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
