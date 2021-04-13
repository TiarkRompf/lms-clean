/*****************************************
Emitting C Generated Code
*******************************************/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "scanner_header.h"
/************* Functions **************/
__global__ void x5(int* x6, int* x7, int x8, int x9, int x10) {
  // this is the permutation kernel for [2, 1, 0]
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimX x dimY x dimZ)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, dimY, (dimZ+31)/32), dim3(32, 8, 1)>>>
  // each threadblock handles a square at dimX and dimZ
  __shared__ int x11[1056];
  // read data from input array to shared memory
  int x12 = 0;
  while (x12 < 32) {
    int x13 = x12;
    int x14 = blockIdx.x * 32 + threadIdx.x;
    if (x14 < x10 && blockIdx.z * 32 + (threadIdx.y + x13) < x8) x11[33 * (threadIdx.y + x13) + threadIdx.x] = x6[(blockIdx.y + (blockIdx.z * 32 + (threadIdx.y + x13)) * x9) * x10 + x14];
    x12 = x12 + 8;
  }
  // sync threads
  __syncthreads();
  // write date from shared memory to output array
  int x15 = 0;
  while (x15 < 32) {
    int x16 = x15;
    int x17 = blockIdx.z * 32 + threadIdx.x;
    if (x17 < x8 && blockIdx.x * 32 + (threadIdx.y + x16) < x10) x7[(blockIdx.y + (blockIdx.x * 32 + (threadIdx.y + x16)) * x9) * x8 + x17] = x11[33 * threadIdx.x + (threadIdx.y + x16)];
    x15 = x15 + 8;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(260000 * sizeof(int));
  scan_int("golden/permute_kernel_210/input.data", x1, 260000);
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(260000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(260000 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(260000 * sizeof(int));
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(260000 * sizeof(int))));
  x5<<<dim3(4, 20, 5), dim3(32, 8, 1)>>>(x2, x4, 130, 20, 100);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(260000 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array("golden/permute_kernel_210/output.data", x3, 260000);
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
