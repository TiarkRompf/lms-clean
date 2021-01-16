/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include "cublas_header.h"
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(2 * sizeof(float));
  int x2 = 0;
  while (x2 != 2) {
    x1[x2] = 1.0;
    x2 = x2 + 1;
  }
  float* x3 = (float*)malloc(4 * sizeof(float));
  int x4 = 0;
  while (x4 != 4) {
    x3[x4] = 2.0;
    x4 = x4 + 1;
  }
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(4 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x5, x3, (size_t)(4 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(2 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x6, x1, (size_t)(2 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x7 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x7, (size_t)(2 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x7, x1, (size_t)(2 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x8 = (float*)malloc(2 * sizeof(float));
  cublasHandle_t x9;
  CUBLAS_CALL(cublasCreate(&x9));
  float x10 = 1.0;
  float x11 = 1.0;
  CUBLAS_CALL(cublasSgemv(x9, CUBLAS_OP_N, 2, 2, &x10, x5, 2, x6, 1, &x11, x7, 1));
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(2 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("Test GEMV:\n");
  int x12 = 0;
  while (x12 != 2) {
    printf("%f, ", x8[x12]);
    x12 = x12 + 1;
  }
  CUDA_CALL(cudaFree(x5));
  CUDA_CALL(cudaFree(x6));
  CUDA_CALL(cudaFree(x7));
  CUBLAS_CALL(cublasDestroy(x9));
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
