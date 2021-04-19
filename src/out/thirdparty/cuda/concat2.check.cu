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
__global__ void x9(float** x10, float* x11) {
  // this is cuda 2-section concat kernel.
  // It concatenates 2 3D arrays on the innermost dimension (dim2).
  // arg0: array of input input arrays
  // arg1: output array
  // call constraint: in.size = 2
  // call constraint: sum of in(i).size = out.size for i in [0, 2)
  int x12 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x12 < 48) {
    int x13 = x12 % 8;
    if (x13 < 3) x11[x12] = x10[0][x12 / 8 * 3 + x13];
    else x11[x12] = x10[1][x12 / 8 * 5 + (x13 - 3)];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(18 * sizeof(float));
  scan_float("golden/concat2/input0.data", x1, 18);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(18 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(18 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(30 * sizeof(float));
  scan_float("golden/concat2/input1.data", x3, 30);
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(30 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x4, x3, (size_t)(30 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(48 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(48 * sizeof(float))));
  float** x7 = (float**)malloc(2 * sizeof(float*));
  x7[0] = x2;
  x7[1] = x4;
  float** x8 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(2 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(2 * sizeof(float*)), cudaMemcpyHostToDevice));
  x9<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x8, x6);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(48 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check general cuda3DConcat kernel
  check_float_array("golden/concat2/output.data", x5, 48);
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
