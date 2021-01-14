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
  printf("create pooling descriptor\n");
  cudnnPoolingDescriptor_t x6;
  CUDNNCHECK(cudnnCreatePoolingDescriptor(&x6));
  CUDNNCHECK(cudnnSetPooling2dDescriptor(x6, CUDNN_POOLING_MAX, CUDNN_PROPAGATE_NAN, 3, 3, 1, 1, 1, 1));
  int x7 = 0;
  int x8 = 0;
  int x9 = 0;
  int x10 = 0;
  CUDNNCHECK(cudnnGetPooling2dForwardOutputDim(x6, x5, &x7, &x8, &x9, &x10));
  printf("Output Image: %d x %d x %d\n", x9, x10, x8);
  int x11 = x8 * x9 * x10;
  printf("create output descriptor\n");
  cudnnTensorDescriptor_t x12;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x12));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x12, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, x9, x10));
  printf("allocate memory for input image and copy\n");
  float* x13 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x13, (size_t)(81 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x13, x1, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  printf("allocate memory for output image\n");
  float* x14 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x14, (size_t)(x11 * sizeof(float))));
  CUDA_CALL(cudaMemset(x14, 0, (size_t)(x11 * sizeof(float))));
  float x15 = 1.0;
  float x16 = 0.0;
  CUDNNCHECK(cudnnPoolingForward(x4, x6, &x15, x5, x13, &x16, x12, x14));
  float* x17 = (float*)malloc(x11 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x17, x14, (size_t)(x11 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("print output:\n");
  int x18 = 0;
  while (x18 != x11) {
    printf("%f, ", x17[x18]);
    x18 = x18 + 1;
  }
  CUDA_CALL(cudaFree(x13));
  CUDA_CALL(cudaFree(x14));
  CUDNNCHECK(cudnnDestroyTensorDescriptor(x5));
  CUDNNCHECK(cudnnDestroyTensorDescriptor(x12));
  CUDNNCHECK(cudnnDestroyPoolingDescriptor(x6));
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
