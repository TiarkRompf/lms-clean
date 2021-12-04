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
__global__ void x4(int x5, int x6, int x7) {
  int x8 = gridDim.x * blockDim.x;
  int x9 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x9 < x7) {
    x6[x9] = x2[x9];
    x9 = x9 + x8;
  }
}
__global__ void x11(int x12, int x13, int x14) {
  int x15 = gridDim.x * blockDim.x;
  int x16 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x16 < x14) {
    x13[x16] = x2[x16] - 1;
    x16 = x16 + x15;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(10 * sizeof(int));
  scan_int_array(x1, 10, "input");
  CUDA_CALL(cudaSetDevice(0));
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(10 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(10 * sizeof(int)), cudaMemcpyHostToDevice));
  int* x3 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(10 * sizeof(int))));
  int* x10 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(10 * sizeof(int))));
  show_tensor(x4<<<dim3(0, 1, 1), dim3(0, 1, 1)>>>(x1, x3, 10));
  show_tensor(x11<<<dim3(0, 1, 1), dim3(0, 1, 1)>>>(x1, x10, 10));
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
