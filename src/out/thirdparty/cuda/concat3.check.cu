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
__global__ void x11(float** x12, float* x13) {
  // this is cuda 3-section concat kernel for 3D inputs at axis 2.
  // It concatenates 3 3D arrays on the innermost dimension (dim2).
  // arg0: array of input input arrays
  // arg1: output array
  // call constraint: in.size = 3
  // call constraint: sum of in(i).size = out.size for i in [0, 3)
  int x14 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x14 < 36) {
    int x15 = x14 % 6;
    if (x15 < 3) x13[x14] = x12[0][x14 / 6 * 3 + x15];
    else if (x15 < 5) x13[x14] = x12[1][x14 / 6 * 2 + (x15 - 3)];
    else x13[x14] = x12[2][x14 / 6 + (x15 - 5)];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(18 * sizeof(float));
  scan_floats("golden/concat3/input0.data", x1, 18);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(18 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(18 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(12 * sizeof(float));
  scan_floats("golden/concat3/input1.data", x3, 12);
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(12 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x4, x3, (size_t)(12 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(6 * sizeof(float));
  scan_floats("golden/concat3/input2.data", x5, 6);
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(6 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x6, x5, (size_t)(6 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x7 = (float*)malloc(30 * sizeof(float));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(30 * sizeof(float))));
  float** x9 = (float**)malloc(3 * sizeof(float*));
  x9[0] = x2;
  x9[1] = x4;
  x9[2] = x6;
  float** x10 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(3 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x10, x9, (size_t)(3 * sizeof(float*)), cudaMemcpyHostToDevice));
  x11<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x10, x8);
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(30 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/concat3/output.data", x7, 30);
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
