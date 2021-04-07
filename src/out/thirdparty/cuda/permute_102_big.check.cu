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
  // this is the permute kernel for [1, 0, 2]
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimY x dimZ x dimX)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3(dimY, dimZ, 1), dim3(A, 1, 1)>>> where A < dimX
  // each threadblock hands one dimX in coalease size of A, then we have dimZ x dimY threadblocks
  // this kernel might be inefficient if the dimX is small. TODO
  int x11 = blockDim.x;
  int x12 = 0;
  while (x12 < x10) {
    int x13 = x12;
    x7[(blockIdx.y + blockIdx.x * x8) * x10 + (threadIdx.x + x13)] = x6[(blockIdx.x + blockIdx.y * x9) * x10 + (threadIdx.x + x13)];
    x12 = x12 + x11;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(1440000 * sizeof(int));
  scan_int("golden/permute_kernel_102_big/input.data", x1, 1440000);
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(1440000 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(1440000 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(1440000 * sizeof(int));
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(1440000 * sizeof(int))));
  x5<<<dim3(60, 40, 1), dim3(100, 1, 1)>>>(x2, x4, 40, 60, 600);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(1440000 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array("golden/permute_kernel_102_big/output.data", x3, 1440000);
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
