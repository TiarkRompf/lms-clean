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
/**************** Snippet ****************/
void Snippet(int x0) {
  float x1[6] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  float x2[6] = { 6.0, 5.0, 4.0, 3.0, 2.0, 1.0 };
  float* x3 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(6 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x3, x1, (size_t)(6 * sizeof(int)), cudaMemcpyHostToDevice));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(6 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x4, x2, (size_t)(6 * sizeof(int)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(6 * sizeof(int))));
  x6<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x3, x4, x5, 6);
  float* x14 = (float*)malloc(6 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x14, x5, (size_t)(6 * sizeof(int)), cudaMemcpyDeviceToHost));
  int x15 = 0;
  while (x15 != 6) {
    printf("%f ", x14[x15]);
    x15 = x15 + 1;
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
