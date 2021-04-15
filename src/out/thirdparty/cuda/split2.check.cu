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
__global__ void x7(float* x8, int x9, float* x10, int x11, float* x12, int x13) {
  // this is cuda 2-way split kernel.
  // It takes a 3D array and splits on the innermost dimension (dim2) into two arrays.
  // in: input array
  // d_other: product of other two dimensions (dim0 * dim1)
  // out0: first output array
  // d0: dim2 of out0
  // out1: second output array
  // d1: dim2 of out1
  // call constraint: d0 + d1 = in.dim2
  int x14 = blockIdx.x * blockDim.x + threadIdx.x;
  int x15 = x11 + x13;
  if (x14 < x9 * x15) {
    int x16 = x14 % x15;
    if (x16 < x11) x10[x14 / x15 * x11 + x16] = x8[x14];
    else x12[x14 / x15 * x13 + (x16 - x11)] = x8[x14];
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
  x7<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, 4, x4, 2, x6, 2);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/split2/output0.data", x3, 8);
  check_float_array("golden/split2/output1.data", x5, 8);
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
