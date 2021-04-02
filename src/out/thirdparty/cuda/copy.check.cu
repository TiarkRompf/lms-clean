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
  // Cuda Matrix Copy
  // arg0: 2D Input Matrix (n x n) where n is a multiple of 32
  // arg1: 2D Output Matrix (n x n) where n is a multiple of 32
  int x10 = blockIdx.x * 32 + threadIdx.x;
  int x11 = blockIdx.y * 32 + threadIdx.y;
  int x12 = gridDim.x * 32;
  int x13 = 0;
  while (x13 < 32) {
    int x14 = (x11 + x13) * x12 + x10;
    x9[x14] = x8[x14];
    x13 = x13 + 8;
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
  int x15 = 0;
  while (x15 != 64) {
    int x16 = 0;
    int x17 = 64 * x15;
    while (x16 != 64) {
      int x18 = x17 + x16;
      if (x1[x18] != x2[x18]) {
        printf("Copy Incorrect!\n");
        fflush(stdout); fflush(stderr); exit(1);
      }
      x16 = x16 + 1;
    }
    x15 = x15 + 1;
  }
  printf("Copy Correct\n");
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
