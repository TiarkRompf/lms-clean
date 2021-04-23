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
__global__ void x9(float* x10, float** x11) {
  // This is cuda 2-section split kernel for 3D input at axis 2.
  // It takes a 3D array and splits on the innermost dimension (dim2) into 2 arrays.
  // arg0: input array
  // arg1: array of output arrays
  // call constraint: sum of out(i).size = in.size for i in [0, 2)
  int x12 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x12 < 16) {
    float x13 = x10[x12];
    int x14 = x12 % 4;
    if (x14 < 2) x11[0][x12 / 4 * 2 + x14] = x13;
    else x11[1][x12 / 4 * 2 + (x14 - 2)] = x13;
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
  float** x7 = (float**)malloc(2 * sizeof(float*));
  x7[0] = x4;
  x7[1] = x6;
  float** x8 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(2 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(2 * sizeof(float*)), cudaMemcpyHostToDevice));
  x9<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x8);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check cuda3DSplit kernel of section 2 against individual outputs
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
