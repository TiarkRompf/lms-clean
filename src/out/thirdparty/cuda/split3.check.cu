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
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(15 * sizeof(float));
  int x2 = 0;
  while (x2 != 15) {
    int x3 = x2;
    x1[x3] = (float)x3;
    x2 = x2 + 1;
  }
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(15 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x4, x1, (size_t)(15 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(6 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(6 * sizeof(float))));
  float* x7 = (float*)malloc(6 * sizeof(float));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(6 * sizeof(float))));
  float* x9 = (float*)malloc(3 * sizeof(float));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(3 * sizeof(float))));
  x11<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x4, 3, x6, 2, x8, 2, x10, 1);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x7, x8, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x9, x10, (size_t)(3 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("output0: [");
  int x24 = 0;
  while (x24 != 6) {
    printf("%f,", x5[x24]);
    x24 = x24 + 1;
  }
  printf("]\n");
  printf("output1: [");
  int x25 = 0;
  while (x25 != 6) {
    printf("%f,", x7[x25]);
    x25 = x25 + 1;
  }
  printf("]\n");
  printf("output2: [");
  int x26 = 0;
  while (x26 != 3) {
    printf("%f,", x9[x26]);
    x26 = x26 + 1;
  }
  printf("]\n");
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
