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
  // arg0: 2D Input Matrix (dimY x dimX) may not be multiples of 32
  // arg1: 2D Output Matrix (dimX x dimY)
  // caller must use <<<dim3((dimX+31)/32, (dimY+31)/32, 1), dim3(32, 8, 1)>>>
  // using gridDimX=(dimX+31)/32, gridDimY=(dimY+31)/32, blockDimX=32, blockDimY=8
  __shared__ int x12[1056];
  // read data from input array to shared memory
  int x13 = 0;
  while (x13 < 32) {
    int x14 = x13;
    int x15 = blockIdx.y * 32 + (threadIdx.y + x14);
    if (x15 < x10 && blockIdx.x * 32 + threadIdx.x < x11) x12[33 * (threadIdx.y + x14) + threadIdx.x] = x8[x15 * x11 + (blockIdx.x * 32 + threadIdx.x)];
    x13 = x13 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x16 = 0;
  while (x16 < 32) {
    int x17 = x16;
    int x18 = blockIdx.x * 32 + (threadIdx.y + x17);
    if (x18 < x11 && blockIdx.y * 32 + threadIdx.x < x10) x9[x18 * x10 + (blockIdx.y * 32 + threadIdx.x)] = x12[33 * threadIdx.x + (threadIdx.y + x17)];
    x16 = x16 + 8;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(20000 * sizeof(int));
  int* x2 = (int*)malloc(20000 * sizeof(int));
  int x3 = 0;
  while (x3 != 20000) {
    int x4 = x3;
    x1[x4] = x4 + 1;
    x3 = x3 + 1;
  }
  int* x5 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(20000 * sizeof(int))));
  int* x6 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(20000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x5, x1, (size_t)(20000 * sizeof(int)), cudaMemcpyHostToDevice));
  x7<<<dim3(7, 4, 1), dim3(32, 8, 1)>>>(x5, x6, 100, 200);
  CUDA_CALL(cudaMemcpy(x2, x6, (size_t)(20000 * sizeof(int)), cudaMemcpyDeviceToHost));
  int x19 = 0;
  while (x19 != 200) {
    int x20 = x19;
    int x21 = 0;
    int x22 = 100 * x20;
    while (x21 != 100) {
      int x23 = x21;
      if (x1[200 * x23 + x20] != x2[x22 + x23]) {
        printf("Transpose Incorrect!\n");
        fflush(stdout); fflush(stderr); exit(1);
      }
      x21 = x21 + 1;
    }
    x19 = x19 + 1;
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
