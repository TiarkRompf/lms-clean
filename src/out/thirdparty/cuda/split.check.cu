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
__global__ void x7(float* x8, float* x9, float* x10, int x11, int x12, int x13) {
  // this is cuda split kernel. It takes a 3D array and split on the innermost dimension.
  // in: input array
  // out1: first output array
  // out2: second output array
  // d0: dim0 of the input array
  // split: dim0 of the first output array
  int x14 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x14 < x13) {
    int x15 = x14 % x11;
    if (x15 < x12) x9[x14 / x11 * x12 + x15] = x8[x14];
    else x10[x14 / x11 * (x11 - x12) + (x15 - x12)] = x8[x14];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(16 * sizeof(float));
  scan_float("golden/split/input.data", x1, 16);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(16 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(16 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(8 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(8 * sizeof(float))));
  float* x5 = (float*)malloc(8 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(8 * sizeof(float))));
  x7<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x4, x6, 4, 2, 16);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/split/output1.data", x3, 8);
  check_float_array("golden/split/output2.data", x5, 8);
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
