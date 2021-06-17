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
__global__ void x7(int* x8, float* x9, float* x10, int x11, int x12, int x13) {
  // Cuda Embedding Grad
  // arg0: embedding indicies
  // arg1: embedding output gradient
  // arg2: embedding gradient
  // arg3: indicies size
  // arg4: stride size
  // arg5: padding index (-1 if unsure)
  extern __shared__ float x14[];
  float* x15 = x14 + NVIDIA_WARP_SIZE * threadIdx.y;
  int* x16 = (int *)(x14 + NVIDIA_WARP_SIZE * blockDim.y);
  int x17 = threadIdx.x + blockIdx.x * blockDim.x;
  int x18 = blockDim.x * blockDim.y;
  int x19 = 0;
  bool x20 = x17 < x12;
  while (x19 < x11) {
    int x21 = x19;
    int x22 = threadIdx.x + threadIdx.y * blockDim.x;
    int x23 = x21 + x22;
    if (x23 < x11) x16[x22] = x8[x23];
    int x24 = x21 + blockDim.x * blockDim.y;
    int x25 = x24 < x11 ? x24 : x11;
    int x26 = blockDim.y;
    int x27 = x21;
    while (x27 < x25) {
      int x28 = x27;
      __syncthreads();
      int x29 = x28 + threadIdx.y;
      bool x30 = x29 < x11;
      if (x30 && x20 && x16[x29 - x21] != x13) x15[threadIdx.x] = x9[x29 * x12 + x17];
      __syncthreads();
      if (x30 && x16[x29 - x21] != x13) {
        int x31 = x25 - x28;
        int x32 = blockDim.y;
        int x33 = x31 < x32 ? x31 : x32;
        int x34 = x16[x29 - x21];
        int x35 = x34 == x16[x28 - x21 + threadIdx.x] ? 1 : 0;
        if (threadIdx.x >= x33) x35 = 0;
        int x36 = __ballot_sync(-1, x35);
        int x37 = x36;
        x8[x33] = x36;
        int x38 = __ffs(x36) - 1;
        if (threadIdx.y == x38) {
          x37 = x36 ^ 1 << x38;
          while (x37 > 0) {
            int x39 = __ffs(x37) - 1;
            x15[threadIdx.x] = x15[threadIdx.x] + x14[threadIdx.x + NVIDIA_WARP_SIZE * x39];
            x37 = x37 ^ 1 << x39;
          }
          if (x20) {
            int x40 = x34 * x12 + x17;
            x10[x40] = x10[x40] + x15[threadIdx.x];
          }
        }
      }
      x27 = x27 + x26;
    }
    x19 = x19 + x18;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(10 * sizeof(int));
  scan_int("golden/embedding/indices.data", x1, 10);
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(10 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(10 * sizeof(int)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(600 * sizeof(float));
  scan_float("golden/embedding/output_grad.data", x3, 600);
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(600 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x4, x3, (size_t)(600 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(1200 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(1200 * sizeof(float))));
  x7<<<dim3(2, 1, 1), dim3(32, 32, 1), 1024 * sizeof(int) + 1024 * sizeof(float)>>>(x2, x4, x6, 10, 60, -1);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(1200 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/embedding/embedding_grad.data", x5, 1200);
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
