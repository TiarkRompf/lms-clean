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
  // this is cuda concat kernel.
  // It concatenates two 3D arrays and concat on the innermost dimension (dim2).
  // arg0: first input array
  // arg1: dim2 of first input array
  // arg2: second input array
  // arg3: dim2 of second input array
  // arg4: output array
  // arg5: product of other two dimensions (dim0 * dim1)
  // call constraint: arg1 + arg3 = out.dim2
  int x14 = blockIdx.x * blockDim.x + threadIdx.x;
  int x15 = x9 + x11;
  if (x14 < x13 * x15) {
    int x16 = x14 % x15;
    x12[x14] = x16 < x9 ? x8[x14 / x15 * x9 + x16] : x10[x14 / x15 * x11 + (x16 - x9)];
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
  x7<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, 3, x4, 5, x6, 6);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(48 * sizeof(float)), cudaMemcpyDeviceToHost));
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
