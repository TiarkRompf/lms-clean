/*****************************************
Emitting C Generated Code
*******************************************/
#include "cudnn_header.h"
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  printf("setting input image\n");
  float* x1 = (float*)malloc(81 * sizeof(float));
  int x2 = 0;
  while (x2 != 81) {
    int x3 = x2;
    x1[x3] = (float)x3;
    x2 = x2 + 1;
  }
  printf("setting up device\n");
  CUDA_CALL(cudaSetDevice(0));
  printf("create handle\n");
  cudnnHandle_t x4;
  CUDNNCHECK(cudnnCreate(&x4));
  printf("create input descriptor\n");
  cudnnTensorDescriptor_t x5;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x5));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x5, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, 9, 9));
  printf("ask CuDNN for memory of reserve space and states\n");
  size_t x6 = 0;
  CUDNNCHECK(cudnnDropoutGetReserveSpaceSize(x5, &x6));
  size_t x7 = 0;
  CUDNNCHECK(cudnnDropoutGetStatesSize(x4, &x7));
  printf("reserve_bytes: %zu, states_bytes: %zu\n", x6, x7);
  printf("allocate memory for states and reserve space\n");
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, x7));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, x6));
  printf("create dropout descriptor\n");
  cudnnDropoutDescriptor_t x10;
  CUDNNCHECK(cudnnCreateDropoutDescriptor(&x10));
  CUDNNCHECK(cudnnSetDropoutDescriptor(x10, x4, 0.5, x8, x7, 1));
  printf("create output descriptor\n");
  cudnnTensorDescriptor_t x11;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x11));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x11, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, 9, 9));
  printf("allocate memory for input image and copy\n");
  float* x12 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x12, (size_t)(81 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x12, x1, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  printf("allocate memory for output image\n");
  float* x13 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x13, (size_t)(81 * sizeof(float))));
  CUDA_CALL(cudaMemset(x13, 0, (size_t)(81 * sizeof(float))));
  CUDNNCHECK(cudnnDropoutForward(x4, x10, x5, x12, x11, x13, x9, x6));
  float* x14 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x14, x13, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("print output:\n");
  int x15 = 0;
  while (x15 != 81) {
    printf("%f, ", x14[x15]);
    x15 = x15 + 1;
  }
  CUDA_CALL(cudaFree(x12));
  CUDA_CALL(cudaFree(x13));
  CUDA_CALL(cudaFree(x9));
  CUDA_CALL(cudaFree(x8));
  CUDNNCHECK(cudnnDestroyTensorDescriptor(x5));
  CUDNNCHECK(cudnnDestroyTensorDescriptor(x11));
  CUDNNCHECK(cudnnDestroyDropoutDescriptor(x10));
  CUDNNCHECK(cudnnDestroy(x4));
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
