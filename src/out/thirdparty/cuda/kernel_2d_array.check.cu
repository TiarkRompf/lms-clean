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
__global__ void x2(float* x3, float* x4, float* x5, int x6) {
  extern __shared__ float x7[];
  float* x8 = x3 + x6 * blockIdx.x;
  float* x9 = x4 + x6 * blockIdx.x;
  float* x10 = x5 + x6 * blockIdx.x;
  int x11 = threadIdx.x;
  int x12 = blockDim.x;
  float x13 = 0.0;
  int x14 = x11;
  while (x14 < x6) {
    int x15 = x14;
    x13 = x13 + x9[x15] * x10[x15];
    x14 = x14 + x12;
  }
  x7[threadIdx.x] = x13;
  __syncthreads();
  // reduce to the first warp
  float x16 = 0.0;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x17 = threadIdx.x;
    int x18 = NVIDIA_WARP_SIZE;
    int x19 = 0;
    while (x19 != x18) {
      x16 = x16 + x7[x17 * NVIDIA_WARP_SIZE + x19];
      x19 = x19 + 1;
    }
    x7[x17] = x16;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x20 = 0.0;
    int x21 = blockDim.x / NVIDIA_WARP_SIZE;
    int x22 = 0;
    while (x22 != x21) {
      x20 = x20 + x7[x22];
      x22 = x22 + 1;
    }
    x7[0] = x20;
  }
  __syncthreads();
  int x23 = x11;
  while (x23 < x6) {
    int x24 = x23;
    x8[x24] = x10[x24] * (x9[x24] - x7[0]);
    x23 = x23 + x12;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(1 * sizeof(float));
  x2<<<dim3(4, 1, 1), dim3(1024, 1, 1), 4096>>>(x1, x1, x1, 3);
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
