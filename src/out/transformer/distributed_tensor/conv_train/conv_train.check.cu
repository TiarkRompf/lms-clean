/*****************************************
Emitting C Generated Code
*******************************************/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "cudnn_header.h"
#include "nccl_header.h"
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include "cublas_header.h"
#include <stdbool.h>
#include "mpi_header.h"
#include "scanner_header.h"
/************* Functions **************/
__global__ void x11(float* x12, float x13, int x14) {
  // begin generating kernel function for FILL of type Float
  int x15 = gridDim.x * blockDim.x;
  int x16 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x16 < x14) {
    x12[x16] = x13;
    x16 = x16 + x15;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x19(float* x20, float* x21, int x22) {
  // begin generating kernel function for ACCUM of type Float
  int x23 = gridDim.x * blockDim.x;
  int x24 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x24 < x22) {
    int x25 = x24;
    x20[x25] = x20[x25] + x21[x25];
    x24 = x24 + x23;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x26(float* x27, float* x28, float* x29, int x30) {
  // begin generating kernel function for SGD of type Float
  int x31 = gridDim.x * blockDim.x;
  int x32 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x32 < x30) {
    int x33 = x32;
    float x34 = x29[x33] * 0.5 + x28[x33];
    x27[x33] = x27[x33] - x34 * 1.0E-4;
    x29[x33] = x34;
    x32 = x32 + x31;
  }
  // end generating kernel function for SGD of type Float
}
/**************** Snippet ****************/
void Snippet(int x0) {
  // begin setting up the MPI/NCCL environment
  int x1 = 0;
  int x2 = 0;
  MPICHECK(MPI_Init(NULL, NULL));
  MPICHECK(MPI_Comm_rank(MPI_COMM_WORLD, &x2));
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x1));
  MPICHECK(MPI_Barrier(MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(x2));
  ncclUniqueId x3;
  NCCLCHECK(ncclGetUniqueId(&x3));
  MPICHECK(MPI_Bcast(&x3, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, MPI_COMM_WORLD));
  ncclComm_t x4;
  NCCLCHECK(ncclCommInitRank(&x4, x1, x3, x2));
  cudaStream_t x5;
  CUDA_CALL(cudaStreamCreateWithFlags(&x5, cudaStreamNonBlocking));
  int x6 = x2;
  // end setting up the MPI/NCCL environment
  // begin setting up the CUDNN environment
  cudnnHandle_t x7;
  CUDNNCHECK(cudnnCreate(&x7));
  // end setting up the CUDNN environment
  // begin initializing GPU array of size 18 and type Float at device (pre-rename) x39 from binary file weight
  float* x8 = (float*)malloc(18 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(18 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x8, 18);
  CUDA_CALL(cudaMemcpy(x9, x8, (size_t)(18 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 18 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(18 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, 0, 18);
  // end initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(18 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, 0, 18);
  // end initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  int x18 = 0;
  while (x18 != 10) {
    // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
    float* x35 = (float*)malloc(81 * sizeof(float));
    CUDA_CALL(cudaSetDevice(x6));
    float* x36 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x36, (size_t)(81 * sizeof(float))));
    scan_float_rank("golden/input", x6, x35, 81);
    CUDA_CALL(cudaMemcpy(x36, x35, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
    // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
    // begin creating and setting tensor descriptor of shape List(2, 1, 9, 9)
    cudnnTensorDescriptor_t x37;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x37));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x37, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 9, 9));
    // end creating and setting tensor descriptor
    // begin creating and setting filter descriptor of shape List(2, 1, 3, 3)
    cudnnFilterDescriptor_t x38;
    CUDNNCHECK(cudnnCreateFilterDescriptor(&x38));
    CUDNNCHECK(cudnnSetFilter4dDescriptor(x38, CUDNN_DATA_FLOAT, CUDNN_TENSOR_NCHW, 2, 1, 3, 3));
    // end creating and setting filter descriptor
    // begin creating and setting convolution descriptor of padding: List(1, 1), strides: List(1, 1), dilation: List(1, 1)
    cudnnConvolutionDescriptor_t x39;
    CUDNNCHECK(cudnnCreateConvolutionDescriptor(&x39));
    CUDNNCHECK(cudnnSetConvolution2dDescriptor(x39, 1, 1, 1, 1, 1, 1, CUDNN_CROSS_CORRELATION, CUDNN_DATA_FLOAT));
    // end creating and setting convolution descriptor
    // begin creating and setting tensor descriptor of shape List(2, 2, 9, 9)
    cudnnTensorDescriptor_t x40;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x40));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x40, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 2, 9, 9));
    // end creating and setting tensor descriptor
    // begin allocating gpu array for the output of convolution
    CUDA_CALL(cudaSetDevice(x6));
    float* x41 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x41, (size_t)(324 * sizeof(float))));
    // end allocating gpu array for the output of convolution
    // begin finding convolution forward algorithm
    cudnnConvolutionFwdAlgoPerf_t x42;
    int x43 = 0;
    CUDNNCHECK(cudnnFindConvolutionForwardAlgorithm(x7, x37, x38, x39, x40, 1, &x43, &x42));
    cudnnConvolutionFwdAlgo_t x44 = x42.algo;
    // end finding convolution forward algorithm
    // begin finding convolution forward workspace size
    size_t x45 = (size_t)0;
    CUDNNCHECK(cudnnGetConvolutionForwardWorkspaceSize(x7, x37, x38, x39, x40, x44, &x45));
    // begin finding convolution backward workspace size
    // begin allocating gpu array for convolution forward workspace
    CUDA_CALL(cudaSetDevice(x6));
    float* x46 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x46, (size_t)x45));
    // end allocating gpu array for convolution forward workspace
    // begin convolution forward pass
    float x47 = 1.0;
    float x48 = 0.0;
    CUDNNCHECK(cudnnConvolutionForward(x7, &x47, x37, x36, x38, x9, x39, x44, x46, x45, &x48, x40, x41));
    // end convolution forward pass
    // begin checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file loss
    float* x49 = (float*)malloc(162 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x49, x41, (size_t)(162 * sizeof(float)), cudaMemcpyDeviceToHost));
    check_float_array_rank("golden/loss", x6, x49, 162);
    // end checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file loss
    // begin initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    float* x50 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x50, (size_t)(162 * sizeof(float))));
    x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x50, 1, 162);
    // end initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
    // begin allocating gpu array for the gradient of filter of convolution
    CUDA_CALL(cudaSetDevice(x6));
    float* x51 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x51, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the gradient of filter of convolution
    // begin finding convolution backward filter algorithm
    cudnnConvolutionBwdFilterAlgoPerf_t x52;
    int x53 = 0;
    CUDNNCHECK(cudnnFindConvolutionBackwardFilterAlgorithm(x7, x37, x40, x39, x38, 1, &x53, &x52));
    cudnnConvolutionBwdFilterAlgo_t x54 = x52.algo;
    // end finding convolution backward filter algorithm
    // begin finding convolution backward filter workspace size
    size_t x55 = (size_t)0;
    CUDNNCHECK(cudnnGetConvolutionBackwardFilterWorkspaceSize(x7, x37, x40, x39, x38, x54, &x55));
    // end finding convolution backward filter workspace size
    // begin allocating gpu array for convolution backward filter workspace
    CUDA_CALL(cudaSetDevice(x6));
    float* x56 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x56, (size_t)x55));
    // end allocating gpu array for convolution backward filter workspace
    // begin convolution backward filter pass
    float x57 = 1.0;
    float x58 = 0.0;
    CUDNNCHECK(cudnnConvolutionBackwardFilter(x7, &x57, x37, x36, x40, x50, x39, x54, x56, x55, &x58, x38, x51));
    // end convolution backward filter pass
    ncclAllReduce(x51, x51, (size_t)18, ncclFloat32, ncclSum, x4, x5);
    CUDA_CALL(cudaStreamSynchronize(x5));
    // begin computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x240
    CUDA_CALL(cudaSetDevice(x6));
    x19<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x51, 18);
    // end computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x240
    // begin computing SGD on GPU for size 18 and type Float at device (pre-name) x39 with weight x50, grad x67, and momentum x107
    CUDA_CALL(cudaSetDevice(x6));
    x26<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x10, x17, 18);
    // end computing SGD on GPU for size 18 and type Float at device (pre-name) x39 with weight x50, grad x67, and momentum x107
    x18 = x18 + 1;
    cudnnDestroyTensorDescriptor(x40);
    cudnnDestroyFilterDescriptor(x38);
    cudnnDestroyTensorDescriptor(x37);
    cudnnDestroyConvolutionDescriptor(x39);
  }
  if (x6 == 0) {
    // begin copying GPU array x50 to CPU and print for size 18 and type Float
    float* x59 = (float*)malloc(18 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x59, x9, (size_t)(18 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x60 = 0;
    while (x60 != 18) {
      printf("%f ", x59[x60]);
      x60 = x60 + 1;
    }
    printf("\n");
    // end copying GPU array x50 to CPU and print for size 18 and type Float
  }
  CUDNNCHECK(cudnnDestroy(x7));
  MPICHECK(MPI_Finalize());
  NCCLCHECK(ncclCommDestroy(x4));
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
