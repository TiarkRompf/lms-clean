/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x9(float* x10, float* x11, int* x12, float x13, int x14, int x15, int x16, int x17, int x18) {
  int x19 = blockIdx.x * blockDim.x + threadIdx.x;
  int x20 = x19;
  int x21 = blockDim.x * gridDim.x;
  int x22 = x19 / x16;
  int x23 = x22;
  int x24 = x22 * x16;
  int x25 = x19 - x24;
  int x26 = x25 / x17;
  int x27 = x26;
  int x28 = x26 * x17;
  int x29 = x24 + x28 + (x25 - x28);
  while (x29 < x18) {
    x11[x29] = x12[x27 % x15 * x14 + x23 % x14] == 0 ? x10[x29] : x13;
    int x30 = x20 + x21;
    x20 = x30;
    int x31 = x30 / x16;
    x23 = x31;
    int x32 = x31 * x16;
    int x33 = x30 - x32;
    int x34 = x33 / x17;
    x27 = x34;
    int x35 = x34 * x17;
    x29 = x32 + x35 + (x33 - x35);
  }
}
__global__ void x42(float* x43, float* x44, int* x45, int x46, int x47, int x48, int x49, int x50) {
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
    if (x45[x59 % x47 * x46 + x55 % x46] == 0) x44[x61] = x44[x61] + x43[x61];
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
  float* x1 = (float*)malloc(4096 * sizeof(float));
  int* x2 = (int*)malloc(4096 * sizeof(int));
  float* x3 = (float*)malloc(4096 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(4096 * sizeof(float))));
  int* x5 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(4096 * sizeof(int))));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(4096 * sizeof(float))));
  int x7 = 0;
  while (x7 != 4096) {
    int x8 = x7;
    x1[x8] = (float)x8;
    if (x8 % 2 == 0) x2[x8] = 1;
    else x2[x8] = 0;
    x7 = x7 + 1;
  }
  CUDA_CALL(cudaMemcpy(x4, x1, (size_t)(4096 * sizeof(float)), cudaMemcpyHostToDevice));
  CUDA_CALL(cudaMemcpy(x5, x2, (size_t)(4096 * sizeof(int)), cudaMemcpyHostToDevice));
  x9<<<dim3(8, 1, 1), dim3(512, 1, 1)>>>(x4, x6, x5, 0.0, 8, 1, 1, 1, 4096);
  CUDA_CALL(cudaMemcpy(x3, x6, (size_t)(4096 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("masked fill output:\n");
  int x36 = 0;
  while (x36 != 4096) {
    printf("%f,", x3[x36]);
    x36 = x36 + 1;
  }
  printf("\n");
  float* x37 = (float*)malloc(4096 * sizeof(float));
  float* x38 = (float*)malloc(4096 * sizeof(float));
  int x39 = 0;
  while (x39 != 4096) {
    x38[x39] = 1.0;
    x39 = x39 + 1;
  }
  float* x40 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x40, (size_t)(4096 * sizeof(float))));
  float* x41 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x41, (size_t)(4096 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x41, x38, (size_t)(4096 * sizeof(float)), cudaMemcpyHostToDevice));
  x42<<<dim3(8, 1, 1), dim3(512, 1, 1)>>>(x41, x40, x5, 8, 1, 1, 1, 4096);
  CUDA_CALL(cudaMemcpy(x37, x40, (size_t)(4096 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("masked fill input gradient:\n");
  int x68 = 0;
  while (x68 != 4096) {
    printf("%f,", x37[x68]);
    x68 = x68 + 1;
  }
  printf("\n");
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
