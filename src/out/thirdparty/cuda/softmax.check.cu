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
__global__ void x6(float* x7, float* x8, int x9) {
  extern __shared__ float x10[];
  float* x11 = x7 + x9 * blockIdx.x;
  float* x12 = x8 + x9 * blockIdx.x;
  float x13 = -INFINITY;
  int x14 = blockDim.x;
  // thread local reduce
  float x15 = x13;
  int x16 = threadIdx.x;
  while (x16 < x9) {
    float x17 = x15;
    float x18 = x11[x16];
    x15 = x17 < x18 ? x18 : x17;
    x16 = x16 + x14;
  }
  x10[threadIdx.x] = x15;
  __syncthreads();
  // reduce to the first warp
  float x19 = x13;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x20 = threadIdx.x;
    int x21 = NVIDIA_WARP_SIZE;
    int x22 = 0;
    while (x22 != x21) {
      float x23 = x19;
      float x24 = x10[x20 * NVIDIA_WARP_SIZE + x22];
      x19 = x23 < x24 ? x24 : x23;
      x22 = x22 + 1;
    }
    x10[x20] = x19;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x25 = x13;
    int x26 = blockDim.x / NVIDIA_WARP_SIZE;
    int x27 = 0;
    while (x27 != x26) {
      float x28 = x25;
      float x29 = x10[x27];
      x25 = x28 < x29 ? x29 : x28;
      x27 = x27 + 1;
    }
    x10[0] = x25;
  }
  __syncthreads();
  float x30 = 0.0;
  int x31 = blockDim.x;
  int x32 = threadIdx.x;
  while (x32 < x9) {
    int x33 = x32;
    float x34 = expf(x11[x33] - x10[0]);
    x30 = x30 + x34;
    x12[x33] = x34;
    x32 = x32 + x31;
  }
  x10[threadIdx.x] = x30;
  __syncthreads();
  // reduce to the first warp
  float x35 = 0.0;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x36 = threadIdx.x;
    int x37 = NVIDIA_WARP_SIZE;
    int x38 = 0;
    while (x38 != x37) {
      x35 = x35 + x10[x36 * NVIDIA_WARP_SIZE + x38];
      x38 = x38 + 1;
    }
    x10[x36] = x35;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x39 = 0.0;
    int x40 = blockDim.x / NVIDIA_WARP_SIZE;
    int x41 = 0;
    while (x41 != x40) {
      x39 = x39 + x10[x41];
      x41 = x41 + 1;
    }
    x10[0] = x39;
  }
  __syncthreads();
  int x42 = blockDim.x;
  int x43 = threadIdx.x;
  while (x43 < x9) {
    int x44 = x43;
    x12[x44] = x12[x44] / x10[0];
    x43 = x43 + x42;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float x1[12] = { 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 1.0, 5.0, 9.0, 1.0, 9.0, 12.0 };
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(12 * sizeof(float))));
  float* x3 = (float*)malloc(12 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(12 * sizeof(float))));
  float x5[12] = { 0.09, 0.2447, 0.6652, 0.0158, 0.1173, 0.8668, 3.0E-4, 0.01798, 0.9817, 1.5E-5, 0.0474, 0.9525 };
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(12 * sizeof(float)), cudaMemcpyHostToDevice));
  x6<<<dim3(4, 1, 1), dim3(1024, 1, 1), 4096>>>(x2, x4, 3);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(12 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x45 = 0;
  while (x45 != 12) {
    int x46 = x45;
    if (abs(x3[x46] - x5[x46]) > 1.0E-4) printf("Error! Expected: %.3f got %.3f\n", x5[x46], x3[x46]);
    else printf("Matched\n");
    x45 = x45 + 1;
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
