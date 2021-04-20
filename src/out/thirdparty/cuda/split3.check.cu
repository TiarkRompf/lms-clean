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
__global__ void x11(float* x12, float** x13) {
  // This is cuda 3-section split kernel.
  // It takes a 3D array and splits on the innermost dimension (dim2) into 3 arrays.
  // arg0: input array
  // arg1: array of output arrays
  // call constraint: out.size = 3
  // call constraint: sum of out(i).size = in.size for i in [0, 3)
  int x14 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x14 < 20) {
    float x15 = x12[x14];
    int x16 = x14 % 5;
    if (x16 < 2) x13[0][x14 / 5 * 2 + x16] = x15;
    else if (x16 < 4) x13[1][x14 / 5 * 2 + (x16 - 2)] = x15;
    else x13[2][x14 / 5 + (x16 - 4)] = x15;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(20 * sizeof(float));
  scan_float("golden/split3/input.data", x1, 20);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(20 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(20 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(8 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(8 * sizeof(float))));
  float* x5 = (float*)malloc(8 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(8 * sizeof(float))));
  float* x7 = (float*)malloc(4 * sizeof(float));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(4 * sizeof(float))));
  float** x9 = (float**)malloc(3 * sizeof(float*));
  x9[0] = x4;
  x9[1] = x6;
  x9[2] = x8;
  float** x10 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(3 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x10, x9, (size_t)(3 * sizeof(float*)), cudaMemcpyHostToDevice));
  x11<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x10);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(4 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check cuda3DSplit kernel of section 3 against individual outputs
  check_float_array("golden/split3/output0.data", x3, 8);
  check_float_array("golden/split3/output1.data", x5, 8);
  check_float_array("golden/split3/output2.data", x7, 4);
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
