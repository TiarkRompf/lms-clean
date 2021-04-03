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
__global__ void x7(int* x8, int* x9) {
  // Cuda Coalesced Transpose
  // arg0: 2D Input Matrix (n x n) where n is a multiple of 32
  // arg1: 2D Output Matrix (n x n) where n is a multiple of 32
  __shared__ int x10[1056];
  int x11 = blockIdx.x * 32 + threadIdx.x;
  int x12 = blockIdx.y * 32 + threadIdx.y;
  int x13 = gridDim.x * 32;
  int x14 = 0;
  while (x14 < 32) {
    int x15 = x14;
    x10[33 * (threadIdx.y + x15) + threadIdx.x] = x8[(x12 + x15) * x13 + x11];
    x14 = x14 + 8;
  }
  __syncthreads();
  int x16 = blockIdx.y * 32 + threadIdx.x;
  int x17 = blockIdx.x * 32 + threadIdx.y;
  int x18 = 0;
  while (x18 < 32) {
    int x19 = x18;
    x9[(x17 + x19) * x13 + x16] = x10[33 * threadIdx.x + (threadIdx.y + x19)];
    x18 = x18 + 8;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(4096 * sizeof(int));
  int* x2 = (int*)malloc(4096 * sizeof(int));
  int x3 = 0;
  while (x3 != 4096) {
    int x4 = x3;
    x1[x4] = x4 + 1;
    x3 = x3 + 1;
  }
  int* x5 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(4096 * sizeof(int))));
  int* x6 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(4096 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x5, x1, (size_t)(4096 * sizeof(int)), cudaMemcpyHostToDevice));
  x7<<<dim3(2, 2, 1), dim3(32, 8, 1)>>>(x5, x6);
  CUDA_CALL(cudaMemcpy(x2, x6, (size_t)(4096 * sizeof(int)), cudaMemcpyDeviceToHost));
  int x20 = 0;
  while (x20 != 64) {
    int x21 = x20;
    int x22 = 0;
    int x23 = 64 * x21;
    while (x22 != 64) {
      int x24 = x22;
      if (x1[x23 + x24] != x2[64 * x24 + x21]) {
        printf("Transpose Incorrect!\n");
        fflush(stdout); fflush(stderr); exit(1);
      }
      x22 = x22 + 1;
    }
    x20 = x20 + 1;
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
