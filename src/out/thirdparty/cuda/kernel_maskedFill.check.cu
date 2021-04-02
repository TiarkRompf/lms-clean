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
  // `ijSwapped` is true if dim0 > dim1, in this case dim0 and dim1 are swapped
  // arg0: 2D in: input array of size `input_size`
  // arg1: 2D out: output array of size `input_size`
  // arg2: 2D out: output array of size input_size`
  // arg3: value: the value to fill
  // arg4: dim0_shape: shape of dim0
  // arg5: dim1_shape: shape of dim1
  // arg6: dim0_stride: stride of dim0
  // arg7: dim1_stride: stride of dim1
  // arg8: input_size: number of elements of arg0, arg1 and arg2
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
    x9[x27] = x10[x25 % x13 * x12 + x21 % x12] == 0 ? x8[x27] : x11;
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
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(4096 * sizeof(float));
  scan_float("golden/emaskedFill/input.data", x1, 4096);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(4096 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(4096 * sizeof(float)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(4096 * sizeof(int));
  scan_int("golden/emaskedFill/mask.data", x3, 4096);
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(4096 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x4, x3, (size_t)(4096 * sizeof(int)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(4096 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(4096 * sizeof(float))));
  x7<<<dim3(8, 1, 1), dim3(512, 1, 1)>>>(x2, x6, x4, 0.0, 64, 64, 64, 1, 4096);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(4096 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/emaskedFill/output.data", x5, 4096);
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
