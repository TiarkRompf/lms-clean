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
__global__ void x7(int* x8, int* x9, int x10, int x11) {
  // this is the transpose kernel
  // arg0: 2D Input Matrix (dimY x dimX) where dimY and dimX are multiples of 32
  // arg1: 2D Output Matrix (dimX x dimY)
  // caller must use <<<dim3(dimX/32, dimY/32, 1), dim3(32, 32, 1)>>>
  // using gridDimX=dimX/32, gridDimY=dimY/32, blockDimX=32, blockDimY=32
  __shared__ int x12[1056];
  // read data from input array to shared memory
  x12[33 * threadIdx.y + threadIdx.x] = x8[(blockIdx.y * 32 + threadIdx.y) * x11 + (blockIdx.x * 32 + threadIdx.x)];
  // sync threads
  __syncthreads();
  // write date from shared memory to output array
  x9[(blockIdx.x * 32 + threadIdx.y) * x10 + (blockIdx.y * 32 + threadIdx.x)] = x12[33 * threadIdx.x + threadIdx.y];
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(8192 * sizeof(int));
  int* x2 = (int*)malloc(8192 * sizeof(int));
  int x3 = 0;
  while (x3 != 8192) {
    int x4 = x3;
    x1[x4] = x4 + 1;
    x3 = x3 + 1;
  }
  int* x5 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(8192 * sizeof(int))));
  int* x6 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(8192 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x5, x1, (size_t)(8192 * sizeof(int)), cudaMemcpyHostToDevice));
  x7<<<dim3(4, 2, 1), dim3(32, 32, 1)>>>(x5, x6, 64, 128);
  CUDA_CALL(cudaMemcpy(x2, x6, (size_t)(8192 * sizeof(int)), cudaMemcpyDeviceToHost));
  int x13 = 0;
  while (x13 != 128) {
    int x14 = x13;
    int x15 = 0;
    int x16 = 64 * x14;
    while (x15 != 64) {
      int x17 = x15;
      if (x1[128 * x17 + x14] != x2[x16 + x17]) {
        printf("Transpose Incorrect!\n");
        fflush(stdout); fflush(stderr); exit(1);
      }
      x15 = x15 + 1;
    }
    x13 = x13 + 1;
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
