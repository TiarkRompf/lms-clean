/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x6(float* x7, float* x8, float* x9, int x10) {
  int x11 = gridDim.x * blockDim.x;
  int x12 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x12 < x10) {
    int x13 = x12;
    x9[x13] = x7[x13] + x8[x13];
    x12 = x12 + x11;
  }
}
__global__ void x15(float* x16, float* x17, float* x18, int x19) {
  int x20 = gridDim.x * blockDim.x;
  int x21 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x21 < x19) {
    int x22 = x21;
    x18[x22] = x16[x22] - x17[x22];
    x21 = x21 + x20;
  }
}
__global__ void x24(float* x25, float* x26, float* x27, int x28) {
  int x29 = gridDim.x * blockDim.x;
  int x30 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x30 < x28) {
    int x31 = x30;
    x27[x31] = x25[x31] * x26[x31];
    x30 = x30 + x29;
  }
}
__global__ void x33(float* x34, float* x35, float* x36, int x37) {
  int x38 = gridDim.x * blockDim.x;
  int x39 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x39 < x37) {
    int x40 = x39;
    x36[x40] = x34[x40] / x35[x40];
    x39 = x39 + x38;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float x1[6] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  float x2[6] = { 6.0, 5.0, 4.0, 3.0, 2.0, 1.0 };
  float* x3 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(6 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x3, x1, (size_t)(6 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(6 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x4, x2, (size_t)(6 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(6 * sizeof(float))));
  x6<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x3, x4, x5, 6);
  float* x14 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x14, (size_t)(6 * sizeof(float))));
  x15<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x3, x4, x14, 6);
  float* x23 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x23, (size_t)(6 * sizeof(float))));
  x24<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x3, x4, x23, 6);
  float* x32 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x32, (size_t)(6 * sizeof(float))));
  x33<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x3, x4, x32, 6);
  float* x41 = (float*)malloc(6 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x41, x5, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x42 = 0;
  while (x42 != 6) {
    printf("%f ", x41[x42]);
    x42 = x42 + 1;
  }
  float* x43 = (float*)malloc(6 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x43, x14, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x44 = 0;
  while (x44 != 6) {
    printf("%f ", x43[x44]);
    x44 = x44 + 1;
  }
  float* x45 = (float*)malloc(6 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x45, x23, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x46 = 0;
  while (x46 != 6) {
    printf("%f ", x45[x46]);
    x46 = x46 + 1;
  }
  float* x47 = (float*)malloc(6 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x47, x32, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x48 = 0;
  while (x48 != 6) {
    printf("%f ", x47[x48]);
    x48 = x48 + 1;
  }
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
