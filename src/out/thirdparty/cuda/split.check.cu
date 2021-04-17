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
__global__ void x5(float* x6, float* x7) {
  int x8 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x8 < 20) {
    int x9 = x8 % 5;
    if (x9 < 2) x7[x8 / 5 * 2 + x9] = x6[x8];
    else if (x9 < 4) x7[8 + x8 / 5 * 2 + (x9 - 2)] = x6[x8];
    else x7[16 + x8 / 5 + (x9 - 4)] = x6[x8];
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(20 * sizeof(float));
  scan_float("golden/split3/input.data", x1, 20);
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(20 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(20 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x3 = (float*)malloc(20 * sizeof(float));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(20 * sizeof(float))));
  x5<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x2, x4);
  CUDA_CALL(cudaMemcpy(x3, x4, (size_t)(20 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array("golden/split3/output.data", x3, 20);
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
