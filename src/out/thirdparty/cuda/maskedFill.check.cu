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
__global__ void x7(float* x8, float* x9, int* x10, float x11, int x12, int x13, int x14, int x15, int x16) {
  // this is the cuda masked fill kernel.
  // The kernel takes an N-d input tensor `in` and selects two dimensions `i` and `j` to work with.
  // `ijSwapped` is true if and only if i > j.
  // `dim0_shape` and `dim1_shape` are shapes of dimension `i` and `j` in input tensor, respectively.
  // `dim0_stide` and `dim1_stide` denote the physical distance between two logically contigent elements
  // in `i` and `j` of the input array, respectively.
  // The kernel also takes a 2-d `mask` tensor, of shape (dim0_shape, dim1_shape). This mask tensor
  // contains only zeros and ones. The kernel fills elements of input tensor with `value` where mask is
  // zero and stores the result to `out`.
  int x17 = blockIdx.x * blockDim.x + threadIdx.x;
  int x18 = x17;
  int x19 = blockDim.x * gridDim.x;
  int x20 = x17 / x14;
  int x21 = x20;
  int x22 = x20 * x14;
  int x23 = x17 - x22;
  int x24 = x23 / x15;
  int x25 = x24;
  int x26 = x24 * x15;
  int x27 = x22 + x26 + (x23 - x26);
  while (x27 < x16) {
    x9[x27] = x10[x21 % x12 * x13 + x25 % x13] == 0 ? x8[x27] : x11;
    int x28 = x18 + x19;
    x18 = x28;
    int x29 = x28 / x14;
    x21 = x29;
    int x30 = x29 * x14;
    int x31 = x28 - x30;
    int x32 = x31 / x15;
    x25 = x32;
    int x33 = x32 * x15;
    x27 = x30 + x33 + (x31 - x33);
  }
}
__global__ void x35(float* x36, float x37, int x38) {
  int x39 = gridDim.x * blockDim.x;
  int x40 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x40 < x38) {
    x36[x40] = x37;
    x40 = x40 + x39;
  }
}
__global__ void x42(float* x43, float* x44, int* x45, int x46, int x47, int x48, int x49, int x50) {
  // this is the cuda masked fill gradient kernel.
  // arg0: gradient of N-d output tensor.
  // arg1: gradient of N-d input tensor.
  // Other parameters are same as maskedFill
  int x51 = blockIdx.x * blockDim.x + threadIdx.x;
  int x52 = x51;
  int x53 = blockDim.x * gridDim.x;
  int x54 = x51 / x48;
  int x55 = x54;
  int x56 = x54 * x48;
  int x57 = x51 - x56;
  int x58 = x57 / x49;
  int x59 = x58;
  int x60 = x58 * x49;
  int x61 = x56 + x60 + (x57 - x60);
  while (x61 < x50) {
    if (x45[x55 % x46 * x47 + x59 % x47] == 0) x44[x61] = x44[x61] + x43[x61];
    int x62 = x52 + x53;
    x52 = x62;
    int x63 = x62 / x48;
    x55 = x63;
    int x64 = x63 * x48;
    int x65 = x62 - x64;
    int x66 = x65 / x49;
    x59 = x66;
    int x67 = x66 * x49;
    x61 = x64 + x67 + (x65 - x67);
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(64 * sizeof(float));
  scan_float("golden/maskedFill/input.data", x1, 64);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(64 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(64 * sizeof(float)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(64 * sizeof(int));
  scan_int("golden/maskedFill/mask.data", x3, 64);
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(64 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x4, x3, (size_t)(64 * sizeof(int)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(64 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(64 * sizeof(float))));
  x7<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x6, x4, 0.0, 8, 8, 8, 1, 64);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(64 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/maskedFill/output.data", x5, 64);
  float* x34 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x34, (size_t)(64 * sizeof(float))));
  x35<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x34, 1.0, 64);
  float* x41 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x41, (size_t)(64 * sizeof(float))));
  x42<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x34, x41, x4, 8, 8, 8, 1, 64);
  float* x68 = (float*)malloc(64 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x68, x41, (size_t)(64 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/maskedFill/input_grad.data", x68, 64);
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
