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
__global__ void x9(float* x10, float* x11, int x12) {
  // This is cuda softmax (for larger; >=1024 inputs). Performs softmax on last dim.
  // arg0: input: <outerSize x lastDimSize>
  // arg1: output: <outerSize x lastDimSize>
  // arg2: lastDimSize: size of the last dimension (i.e., the softmax dim)
  // invocation assumption: <<<dim3(outerSize,1,1), dim3(1024,1,1), 1024*4>>>
  extern __shared__ float x13[];
  float* x14 = x10 + x12 * blockIdx.x;
  float* x15 = x11 + x12 * blockIdx.x;
  float x16 = -INFINITY;
  int x17 = blockDim.x;
  // thread local reduce
  float x18 = x16;
  int x19 = threadIdx.x;
  while (x19 < x12) {
    float x20 = x18;
    float x21 = x14[x19];
    x18 = x20 < x21 ? x21 : x20;
    x19 = x19 + x17;
  }
  x13[threadIdx.x] = x18;
  __syncthreads();
  // reduce to the first warp
  float x22 = x16;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x23 = threadIdx.x;
    int x24 = NVIDIA_WARP_SIZE;
    int x25 = 0;
    while (x25 != x24) {
      float x26 = x22;
      float x27 = x13[x23 * NVIDIA_WARP_SIZE + x25];
      x22 = x26 < x27 ? x27 : x26;
      x25 = x25 + 1;
    }
    x13[x23] = x22;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x28 = x16;
    int x29 = blockDim.x / NVIDIA_WARP_SIZE;
    int x30 = 0;
    while (x30 != x29) {
      float x31 = x28;
      float x32 = x13[x30];
      x28 = x31 < x32 ? x32 : x31;
      x30 = x30 + 1;
    }
    x13[0] = x28;
  }
  __syncthreads();
  float x33 = 0.0;
  int x34 = blockDim.x;
  int x35 = threadIdx.x;
  while (x35 < x12) {
    int x36 = x35;
    float x37 = expf(x14[x36] - x13[0]);
    x33 = x33 + x37;
    x15[x36] = x37;
    x35 = x35 + x34;
  }
  x13[threadIdx.x] = x33;
  __syncthreads();
  // reduce to the first warp
  float x38 = 0.0;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x39 = threadIdx.x;
    int x40 = NVIDIA_WARP_SIZE;
    int x41 = 0;
    while (x41 != x40) {
      x38 = x38 + x13[x39 * NVIDIA_WARP_SIZE + x41];
      x41 = x41 + 1;
    }
    x13[x39] = x38;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x42 = 0.0;
    int x43 = blockDim.x / NVIDIA_WARP_SIZE;
    int x44 = 0;
    while (x44 != x43) {
      x42 = x42 + x13[x44];
      x44 = x44 + 1;
    }
    x13[0] = x42;
  }
  __syncthreads();
  int x45 = blockDim.x;
  int x46 = threadIdx.x;
  while (x46 < x12) {
    int x47 = x46;
    x15[x47] = x15[x47] / x13[0];
    x46 = x46 + x45;
  }
}
__global__ void x48(float* x49, float* x50, float* x51, int x52) {
  // This is cuda softmax (for larger; >=1024 inputs). Performs softmax on last dim.
  // arg0: gradInput: the gradient of the original input (i.e., the softmax input) - This is an output of the kernel
  // arg1: gradOutput: gradient of softmax output, coming from upstream; An Input to the kernel
  // arg2: output: output of softmax forward pass
  // arg3: size: last dimension size
  // invocation assumption: <<<dim3(outerSize,1,1), dim3(1024,1,1), 1024*4>>>
  extern __shared__ float x53[];
  float* x54 = x49 + x52 * blockIdx.x;
  float* x55 = x50 + x52 * blockIdx.x;
  float* x56 = x51 + x52 * blockIdx.x;
  int x57 = threadIdx.x;
  int x58 = blockDim.x;
  float x59 = 0.0;
  int x60 = x57;
  while (x60 < x52) {
    int x61 = x60;
    x59 = x59 + x55[x61] * x56[x61];
    x60 = x60 + x58;
  }
  x53[threadIdx.x] = x59;
  __syncthreads();
  // reduce to the first warp
  float x62 = 0.0;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x63 = threadIdx.x;
    int x64 = NVIDIA_WARP_SIZE;
    int x65 = 0;
    while (x65 != x64) {
      x62 = x62 + x53[x63 * NVIDIA_WARP_SIZE + x65];
      x65 = x65 + 1;
    }
    x53[x63] = x62;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x66 = 0.0;
    int x67 = blockDim.x / NVIDIA_WARP_SIZE;
    int x68 = 0;
    while (x68 != x67) {
      x66 = x66 + x53[x68];
      x68 = x68 + 1;
    }
    x53[0] = x66;
  }
  __syncthreads();
  int x69 = x57;
  while (x69 < x52) {
    int x70 = x69;
    x54[x70] = x56[x70] * (x55[x70] - x53[0]);
    x69 = x69 + x58;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(15640 * sizeof(float));
  scan_floats("golden/softmax/input.data", x1, 15640);
  float* x2 = (float*)malloc(15640 * sizeof(float));
  scan_floats("golden/softmax/output_grad.data", x2, 15640);
  float* x3 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(15640 * sizeof(float))));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(15640 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x3, x1, (size_t)(15640 * sizeof(float)), cudaMemcpyHostToDevice));
  CUDA_CALL(cudaMemcpy(x4, x2, (size_t)(15640 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(15640 * sizeof(float));
  float* x6 = (float*)malloc(15640 * sizeof(float));
  float* x7 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x7, (size_t)(15640 * sizeof(float))));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(15640 * sizeof(float))));
  x9<<<dim3(20, 1, 1), dim3(1024, 1, 1), 4096>>>(x3, x8, 782);
  x48<<<dim3(20, 1, 1), dim3(1024, 1, 1), 4096>>>(x7, x4, x8, 782);
  CUDA_CALL(cudaMemcpy(x5, x7, (size_t)(15640 * sizeof(float)), cudaMemcpyDeviceToHost));
  CUDA_CALL(cudaMemcpy(x6, x8, (size_t)(15640 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/softmax/input_grad.data", x5, 15640);
  check_float_array("golden/softmax/output.data", x6, 15640);
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
