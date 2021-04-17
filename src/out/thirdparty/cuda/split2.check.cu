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
__global__ void x9(float* x10, int x11, float* x12, int x13, float* x14, int x15) {
  // this is cuda 2-way split kernel.
  // It takes a 3D array and splits on the innermost dimension (dim2) into two arrays.
  // arg0: input array
  // arg1: product of other two dimensions (dim0 * dim1)
  // arg2: first output array
  // arg3: dim2 of first output array
  // arg4: second output array
  // arg5: dim2 of second output array
  // call constraint: arg3 + arg5 = arg0.dim2
  int x16 = blockIdx.x * blockDim.x + threadIdx.x;
  int x17 = x13 + x15;
  if (x16 < x11 * x17) {
    int x18 = x16 % x17;
    if (x18 < x13) x12[x16 / x17 * x13 + x18] = x10[x16];
    else x14[x16 / x17 * x15 + (x18 - x13)] = x10[x16];
  }
}
__global__ void x19(float* x20, float* x21) {
  int x22 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x22 < 16) {
    int x23 = x22 % 4;
    if (x23 < 2) x21[x22 / 4 * 2 + x23] = x20[x22];
    else x21[8 + x22 / 4 * 2 + (x23 - 2)] = x20[x22];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(16 * sizeof(float));
  scan_float("golden/split2/input.data", x1, 16);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(16 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(16 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(8 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(8 * sizeof(float))));
  float* x5 = (float*)malloc(8 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(8 * sizeof(float))));
  float* x7 = (float*)malloc(16 * sizeof(float));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(16 * sizeof(float))));
  x9<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, 4, x4, 2, x6, 2);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check cuda3DSplit2 kernel against individual outputs
  check_float_array("golden/split2/output0.data", x3, 8);
  check_float_array("golden/split2/output1.data", x5, 8);
  x19<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x8);
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(16 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check general cuda3DSplit kernel against one-array output
  check_float_array("golden/split2/output.data", x7, 16);
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
