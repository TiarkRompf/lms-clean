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
__global__ void x11(float* x12, int x13, float* x14, int x15, float* x16, int x17, float* x18, int x19) {
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
  int x20 = blockIdx.x * blockDim.x + threadIdx.x;
  int x21 = x15 + x17;
  int x22 = x21 + x19;
  if (x20 < x13 * x22) {
    int x23 = x20 % x22;
    if (x23 < x15) x14[x20 / x22 * x15 + x23] = x12[x20];
    else if (x23 < x21) x16[x20 / x22 * x17 + (x23 - x15)] = x12[x20];
    else x18[x20 / x22 * x19 + (x23 - x21)] = x12[x20];
  }
}
__global__ void x24(float* x25, float* x26) {
  int x27 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x27 < 20) {
    int x28 = x27 % 5;
    if (x28 < 2) x26[x27 / 5 * 2 + x28] = x25[x27];
    else if (x28 < 4) x26[8 + x27 / 5 * 2 + (x28 - 2)] = x25[x27];
    else x26[16 + x27 / 5 + (x28 - 4)] = x25[x27];
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
  float* x9 = (float*)malloc(20 * sizeof(float));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(20 * sizeof(float))));
  x11<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, 4, x4, 2, x6, 2, x8, 1);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(8 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(4 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check cuda3DSplit3 kernel against individual outputs
  check_float_array("golden/split3/output0.data", x3, 8);
  check_float_array("golden/split3/output1.data", x5, 8);
  check_float_array("golden/split3/output2.data", x7, 4);
  x24<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x10);
  CUDA_CALL(cudaMemcpy(x9, x10, (size_t)(20 * sizeof(float)), cudaMemcpyDeviceToHost));
  // check general cuda3DSplit kernel against one-array output
  check_float_array("golden/split3/output.data", x9, 20);
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
