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
__global__ void x9(float* x10, int x11, float* x12, int x13, float* x14, int x15, float* x16, int x17) {
  // this is cuda 3-way split kernel.
  // It takes a 3D array and splits on the innermost dimension (dim2) into three arrays.
  // arg0: input array
  // arg1: product of other two dimensions (dim0 * dim1)
  // arg2: first output array
  // arg3: dim2 of first output array
  // arg4: second output array
  // arg5: dim2 of second output array
  // arg6: third output array
  // arg7: dim2 of third output array
  // call constraint: arg3 + arg5 + arg7 = arg0.dim2
  int x18 = blockIdx.x * blockDim.x + threadIdx.x;
  int x19 = x13 + x15;
  int x20 = x19 + x17;
  if (x18 < x11 * x20) {
    int x21 = x18 % x20;
    if (x21 < x13) x12[x18 / x20 * x13 + x21] = x10[x18];
    else if (x21 < x19) x14[x18 / x20 * x15 + (x21 - x13)] = x10[x18];
    else x16[x18 / x20 * x17 + (x21 - x19)] = x10[x18];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(15 * sizeof(float));
  scan_float("golden/split3/input.data", x1, 15);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(15 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(15 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(6 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(6 * sizeof(float))));
  float* x5 = (float*)malloc(6 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(6 * sizeof(float))));
  float* x7 = (float*)malloc(3 * sizeof(float));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(3 * sizeof(float))));
  x9<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, 3, x4, 2, x6, 2, x8, 1);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(3 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/split3/output0.data", x3, 6);
  check_float_array("golden/split3/output1.data", x5, 6);
  check_float_array("golden/split3/output2.data", x7, 3);
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
