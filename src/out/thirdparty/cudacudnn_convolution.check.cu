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
  printf("setting kernel\n");
  float* x4 = (float*)malloc(9 * sizeof(float));
  x4[0] = 1.0;
  x4[1] = 1.0;
  x4[2] = 1.0;
  x4[3] = 1.0;
  x4[4] = -7.0;
  x4[5] = 1.0;
  x4[6] = 1.0;
  x4[7] = 1.0;
  x4[8] = 1.0;
  printf("setting up device\n");
  CUDA_CALL(cudaSetDevice(0));
  printf("create handle\n");
  cudnnHandle_t x5;
  CUDNNCHECK(cudnnCreate(&x5));
  printf("create input descriptor\n");
  cudnnTensorDescriptor_t x6;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x6));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x6, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, 9, 9));
  printf("create kernel descriptor\n");
  cudnnFilterDescriptor_t x7;
  CUDNNCHECK(cudnnCreateFilterDescriptor(&x7));
  CUDNNCHECK(cudnnSetFilter4dDescriptor(x7, CUDNN_DATA_FLOAT, CUDNN_TENSOR_NCHW, 1, 1, 3, 3));
  printf("create conv descriptor\n");
  cudnnConvolutionDescriptor_t x8;
  CUDNNCHECK(cudnnCreateConvolutionDescriptor(&x8));
  CUDNNCHECK(cudnnSetConvolution2dDescriptor(x8, 1, 1, 1, 1, 1, 1, CUDNN_CONVOLUTION, CUDNN_DATA_FLOAT));
  int x9 = 0;
  int x10 = 0;
  int x11 = 0;
  int x12 = 0;
  CUDNNCHECK(cudnnGetConvolution2dForwardOutputDim(x8, x6, x7, &x9, &x10, &x11, &x12));
  printf("Output Image: %d x %d x %d\n", x11, x12, x10);
  printf("create output descriptor\n");
  cudnnTensorDescriptor_t x13;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x13));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x13, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, 9, 9));
  printf("find conv algorithm\n");
  int x14 = 0;
  cudnnConvolutionFwdAlgoPerf_t x15;
  CUDNNCHECK(cudnnFindConvolutionForwardAlgorithm(x5, x6, x7, x8, x13, 1, &x14, &x15));
  cudnnConvolutionFwdAlgo_t x16 = x15.algo;
  size_t x17 = 0;
  CUDNNCHECK(cudnnGetConvolutionForwardWorkspaceSize(x5, x6, x7, x8, x13, x16, &x17));
  printf("allocate memory for workspace\n");
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, x17));
  printf("allocate memory for input image and copy\n");
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(81 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x19, x1, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  printf("allocate memory for output image\n");
  float* x20 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x20, (size_t)(81 * sizeof(float))));
  CUDA_CALL(cudaMemset(x20, 0, (size_t)(81 * sizeof(float))));
  printf("allocate memory for kernel and copy\n");
  float* x21 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x21, (size_t)(9 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x21, x4, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  float x22 = 1.0;
  float x23 = 0.0;
  printf("convolution\n");
  CUDNNCHECK(cudnnConvolutionForward(x5, &x22, x6, x19, x7, x21, x8, x16, x18, x17, &x23, x13, x20));
  float* x24 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x24, x20, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  printf("print output:\n");
  int x25 = 0;
  while (x25 != 81) {
    printf("%f, ", x24[x25]);
    x25 = x25 + 1;
  }
  CUDA_CALL(cudaFree(x21));
  CUDA_CALL(cudaFree(x19));
  CUDA_CALL(cudaFree(x20));
  CUDA_CALL(cudaFree(x18));
  CUDNNCHECK(cudnnDestroyTensorDescriptor(x6));
  CUDNNCHECK(cudnnDestroyTensorDescriptor(x13));
  CUDNNCHECK(cudnnDestroyFilterDescriptor(x7));
  CUDNNCHECK(cudnnDestroyConvolutionDescriptor(x8));
  CUDNNCHECK(cudnnDestroy(x5));
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
