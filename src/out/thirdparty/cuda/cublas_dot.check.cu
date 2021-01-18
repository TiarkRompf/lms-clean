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
  float* x1 = (float*)malloc(5 * sizeof(float));
  int x2 = 0;
  while (x2 != 5) {
    x1[x2] = 1.0;
    x2 = x2 + 1;
  }
  float* x3 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x3, (size_t)(5 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x3, x1, (size_t)(5 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(5 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x4, x1, (size_t)(5 * sizeof(float)), cudaMemcpyHostToDevice));
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)sizeof(float)));
  cublasHandle_t x6;
  CUBLAS_CALL(cublasCreate(&x6));
  CUBLAS_CALL(cublasSdot(x6, 10, x3, 1, x4, 1, x5));
  float* x7 = (float*)malloc(5 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x7, x5, (size_t)sizeof(float), cudaMemcpyDeviceToHost));
  printf("Test DOT:\n");
  printf("%f\n", x7[0]);
  CUDA_CALL(cudaFree(x3));
  CUDA_CALL(cudaFree(x4));
  CUDA_CALL(cudaFree(x5));
  CUBLAS_CALL(cublasDestroy(x6));
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
