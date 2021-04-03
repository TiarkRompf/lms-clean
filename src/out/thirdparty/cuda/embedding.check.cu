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
__global__ void x7(float* x8, int* x9, float* x10, int x11) {
  // this is cuda embedding kernel.
  // arg0: 2D embedding table: <n_embedding x embed_size>
  // arg1: 1D indices: <indices_size>
  // arg2: 2D output: <indices_size x embed_size>
  // arg3: embed_size
  // invocation assumption: <<<dim3(a,1,1), dim3(indices_size,1,1)>>> where a <= embed_size
  // each thread block handles one embedding vector
  int x12 = blockDim.x;
  int x13 = threadIdx.x;
  int x14 = x9[blockIdx.x] * x11;
  while (x13 < x11) {
    int x15 = x13;
    x10[blockIdx.x * x11 + x15] = x8[x14 + x15];
    x13 = x13 + x12;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(1200 * sizeof(float));
  scan_float("golden/embedding/embedding.data", x1, 1200);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(1200 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(1200 * sizeof(float)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(10 * sizeof(int));
  scan_int("golden/embedding/indices.data", x3, 10);
  int* x4 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(10 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x4, x3, (size_t)(10 * sizeof(int)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(600 * sizeof(float));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(600 * sizeof(float))));
  x7<<<dim3(60, 1, 1), dim3(10, 1, 1)>>>(x2, x4, x6, 60);
  CUDA_CALL(cudaMemcpy(x5, x6, (size_t)(600 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/embedding/output.data", x5, 600);
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
