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
__global__ void x9(int x10, float x11, float* x12, float* x13) {
  // this is cuda saxpy (single-precision A * X plus Y) kernel
  // arg0: size of input array
  int x14 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x14 < x10) x13[x14] = x11 * x12[x14] + x13[x14];
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(4096 * sizeof(float));
  float* x2 = (float*)malloc(4096 * sizeof(float));
  float* x3 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(4096 * sizeof(float))));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(4096 * sizeof(float))));
  int x5 = 0;
  while (x5 != 4096) {
    int x6 = x5;
    x1[x6] = 1.0;
    x2[x6] = 2.0;
    x5 = x5 + 1;
  }
  CUDA_CALL(cudaMemcpy(x3, x1, (size_t)(4096 * sizeof(float)), cudaMemcpyHostToDevice));
  CUDA_CALL(cudaMemcpy(x4, x2, (size_t)(4096 * sizeof(float)), cudaMemcpyHostToDevice));
  cudaEvent_t x7;
  cudaEvent_t x8;
  CUDA_CALL(cudaEventCreate(&x7));
  CUDA_CALL(cudaEventCreate(&x8));
  CUDA_CALL(cudaEventRecord(x7));
  x9<<<dim3(8, 1, 1), dim3(512, 1, 1)>>>(4096, 2.0, x3, x4);
  CUDA_CALL(cudaEventRecord(x8));
  CUDA_CALL(cudaEventSynchronize(x8));
  float x15 = 0.0;
  CUDA_CALL(cudaEventElapsedTime(&x15, x7, x8));
  float x16 = x15;
  CUDA_CALL(cudaMemcpy(x2, x4, (size_t)(4096 * sizeof(float)), cudaMemcpyDeviceToHost));
  float x17 = 0.0;
  int x18 = 0;
  while (x18 != 4096) {
    float x19 = abs(x2[x18] - 4.0);
    if (x19 > x17) x17 = x19;
    x18 = x18 + 1;
  }
  printf("Max error: %f\n", x17);
  printf("Time: %f\n", x16);
  printf("Effective Bandwidth (GB/s): %f\n", (double)(49152.0 / x16) / 1000000.0);
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
