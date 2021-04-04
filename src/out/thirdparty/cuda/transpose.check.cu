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
__global__ void x13(int* x14, int* x15, int x16, int x17) {
  // Cuda Coalesced Transpose
  // arg0: 2D Input Matrix (n x m)
  // arg1: 2D Output Transposed Matrix (m x n)
  __shared__ int x18[1056];
  int x19 = 0;
  while (x19 < 32) {
    int x20 = x19;
    int x21 = blockIdx.x * 32 + threadIdx.x;
    if (x21 < x17 && blockIdx.y * 32 + threadIdx.y + x20 < x16) x18[33 * (threadIdx.y + x20) + threadIdx.x] = x14[(blockIdx.y * 32 + threadIdx.y + x20) * x17 + x21];
    x19 = x19 + 8;
  }
  __syncthreads();
  int x22 = 0;
  while (x22 < 32) {
    int x23 = x22;
    int x24 = blockIdx.y * 32 + threadIdx.x;
    if (x24 < x16 && blockIdx.x * 32 + threadIdx.y + x23 < x17) x15[(blockIdx.x * 32 + threadIdx.y + x23) * x16 + x24] = x18[33 * threadIdx.x + (threadIdx.y + x23)];
    x22 = x22 + 8;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(11928 * sizeof(int));
  int* x2 = (int*)malloc(11928 * sizeof(int));
  int* x3 = (int*)malloc(11928 * sizeof(int));
  int x4 = 0;
  while (x4 != 11928) {
    int x5 = x4;
    x1[x5] = x5;
    x4 = x4 + 1;
  }
  int x6 = 0;
  while (x6 != 56) {
    int x7 = x6;
    int x8 = 0;
    int x9 = x7 * 213;
    while (x8 != 213) {
      int x10 = x8;
      x3[x9 + x10] = x1[x10 * 56 + x7];
      x8 = x8 + 1;
    }
    x6 = x6 + 1;
  }
  int* x11 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(11928 * sizeof(int))));
  int* x12 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x12, (size_t)(11928 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x11, x1, (size_t)(11928 * sizeof(int)), cudaMemcpyHostToDevice));
  x13<<<dim3(2, 7, 1), dim3(32, 8, 1)>>>(x11, x12, 213, 56);
  CUDA_CALL(cudaMemcpy(x2, x12, (size_t)(11928 * sizeof(int)), cudaMemcpyDeviceToHost));
  int x25 = 0;
  while (x25 != 11928) {
    int x26 = x25;
    if (x3[x26] != x2[x26]) {
      printf("Transpose Incorrect!\n");
      fflush(stdout); fflush(stderr); exit(1);
    }
    x25 = x25 + 1;
  }
  printf("Transpose Correct\n");
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
