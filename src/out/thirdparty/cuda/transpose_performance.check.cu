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
__global__ void x9(int* x10, int* x11) {
  // Cuda Matrix Copy
  // arg0: 2D Input Matrix (n x n) where n is a multiple of 32
  // arg1: 2D Output Matrix (n x n) where n is a multiple of 32
  int x12 = blockIdx.x * 32 + threadIdx.x;
  int x13 = blockIdx.y * 32 + threadIdx.y;
  int x14 = gridDim.x * 32;
  int x15 = 0;
  while (x15 < 32) {
    int x16 = (x13 + x15) * x14 + x12;
    x11[x16] = x10[x16];
    x15 = x15 + 8;
  }
}
__global__ void x22(int* x23, int* x24) {
  // Cuda Transpose Naive
  // arg0: 2D Input Matrix (n x n) where n is a multiple of 32
  // arg1: 2D Output Matrix (n x n) where n is a multiple of 32
  int x25 = blockIdx.x * 32 + threadIdx.x;
  int x26 = blockIdx.y * 32 + threadIdx.y;
  int x27 = gridDim.x * 32;
  int x28 = 0;
  int x29 = x25 * x27;
  while (x28 < 32) {
    int x30 = x26 + x28;
    x24[x29 + x30] = x23[x30 * x27 + x25];
    x28 = x28 + 8;
  }
}
__global__ void x36(int* x37, int* x38, int x39, int x40) {
  // Cuda Coalesced Transpose
  // arg0: 2D Input Matrix (n x m)
  // arg1: 2D Output Transposed Matrix (m x n)
  // arg2: number of rows for input matrix
  // arg3: number of columns for input matrix
  // kernel launch config <<dim3((TILE_DIM * m - 1) / TILE_DIM, (TILE_DIM * n - 1) / TILE_DIM), dim3(TILE_DIM, BLOCK_ROWS)>>
  // TILE_DIM = 32, BLOCK_ROWS = 8
  __shared__ int x41[1056];
  int x42 = blockIdx.x * 32 + threadIdx.x;
  int x43 = blockIdx.y * 32 + threadIdx.y;
  int x44 = 0;
  while (x44 < 32) {
    int x45 = x44;
    if (x42 < x40 && x43 < x39) x41[33 * (threadIdx.y + x45) + threadIdx.x] = x37[x43 * x40 + x42];
    x43 = x43 + 8;
    x44 = x44 + 8;
  }
  __syncthreads();
  x42 = blockIdx.y * 32 + threadIdx.x;
  x43 = blockIdx.x * 32 + threadIdx.y;
  int x46 = 0;
  while (x46 < 32) {
    int x47 = x46;
    if (x42 < x39 && x43 < x40) x38[x43 * x39 + x42] = x41[33 * threadIdx.x + (threadIdx.y + x47)];
    x43 = x43 + 8;
    x46 = x46 + 8;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(4096 * sizeof(int));
  int* x2 = (int*)malloc(4096 * sizeof(int));
  int x3 = 0;
  while (x3 != 4096) {
    int x4 = x3;
    x1[x4] = x4 + 1;
    x3 = x3 + 1;
  }
  int* x5 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(4096 * sizeof(int))));
  int* x6 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(4096 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x5, x1, (size_t)(4096 * sizeof(int)), cudaMemcpyHostToDevice));
  cudaEvent_t x7;
  cudaEvent_t x8;
  CUDA_CALL(cudaEventCreate(&x7));
  CUDA_CALL(cudaEventCreate(&x8));
  CUDA_CALL(cudaEventRecord(x7));
  int x17 = 0;
  while (x17 != 100) {
    x9<<<dim3(2, 2, 1), dim3(32, 8, 1)>>>(x5, x6);
    x17 = x17 + 1;
  }
  CUDA_CALL(cudaEventRecord(x8));
  CUDA_CALL(cudaEventSynchronize(x8));
  float x18 = 0.0;
  CUDA_CALL(cudaEventElapsedTime(&x18, x7, x8));
  float x19 = x18;
  CUDA_CALL(cudaMemcpy(x2, x6, (size_t)(4096 * sizeof(int)), cudaMemcpyDeviceToHost));
  cudaEvent_t x20;
  cudaEvent_t x21;
  CUDA_CALL(cudaEventCreate(&x20));
  CUDA_CALL(cudaEventCreate(&x21));
  CUDA_CALL(cudaEventRecord(x20));
  int x31 = 0;
  while (x31 != 100) {
    x22<<<dim3(2, 2, 1), dim3(32, 8, 1)>>>(x5, x6);
    x31 = x31 + 1;
  }
  CUDA_CALL(cudaEventRecord(x21));
  CUDA_CALL(cudaEventSynchronize(x21));
  float x32 = 0.0;
  CUDA_CALL(cudaEventElapsedTime(&x32, x20, x21));
  float x33 = x32;
  CUDA_CALL(cudaMemcpy(x2, x6, (size_t)(4096 * sizeof(int)), cudaMemcpyDeviceToHost));
  cudaEvent_t x34;
  cudaEvent_t x35;
  CUDA_CALL(cudaEventCreate(&x34));
  CUDA_CALL(cudaEventCreate(&x35));
  CUDA_CALL(cudaEventRecord(x34));
  int x48 = 0;
  while (x48 != 100) {
    x36<<<dim3(2, 2, 1), dim3(32, 8, 1)>>>(x5, x6, 64, 64);
    x48 = x48 + 1;
  }
  CUDA_CALL(cudaEventRecord(x35));
  CUDA_CALL(cudaEventSynchronize(x35));
  float x49 = 0.0;
  CUDA_CALL(cudaEventElapsedTime(&x49, x34, x35));
  float x50 = x49;
  CUDA_CALL(cudaMemcpy(x2, x6, (size_t)(4096 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("COPY KERNEL STATS:\n");
  printf("Time: %f ms\n", x19);
  printf("Bandwidth (GB/s): %f\n", 0.008192 / (double)x19);
  printf("=======================\n");
  printf("NAIVE TRANSPOSE KERNEL STATS:\n");
  printf("Time: %f ms\n", x33);
  printf("Bandwidth (GB/s): %f\n", 0.008192 / (double)x33);
  printf("=======================\n");
  printf("COALESCED TRANSPOSE KERNEL STATS:\n");
  printf("Time: %f ms\n", x50);
  printf("Bandwidth (GB/s): %f\n", 0.008192 / (double)x50);
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
