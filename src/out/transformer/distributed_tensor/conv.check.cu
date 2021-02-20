/*****************************************
Emitting C Generated Code
*******************************************/
#include "cudnn_header.h"
#include "nccl_header.h"
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "mpi_header.h"
/************* Functions **************/
__global__ void x12(float* x13, float x14, int x15) {
  int x16 = gridDim.x * blockDim.x;
  int x17 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x17 < x15) {
    x13[x17] = x14;
    x17 = x17 + x16;
  }
}
__global__ void x20(float* x21, float* x22, int x23) {
  // begin generating kernel function for ACCUM of type Float
  int x24 = gridDim.x * blockDim.x;
  int x25 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x25 < x23) {
    int x26 = x25;
    x21[x26] = x21[x26] + x22[x26];
    x25 = x25 + x24;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x27(float* x28, float* x29, float* x30, int x31) {
  // begin generating kernel function for SGD of type Float
  int x32 = gridDim.x * blockDim.x;
  int x33 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x33 < x31) {
    int x34 = x33;
    float x35 = x30[x34] * 0.5 + x29[x34];
    x28[x34] = x28[x34] - x35 * 1.0E-4;
    x30[x34] = x35;
    x33 = x33 + x32;
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
  // begin initializing random GPU array of size 9 and type Float at device (pre-rename) x39
  float* x8 = (float*)malloc(9 * sizeof(float));
  int x9 = 0;
  while (x9 != 9) {
    x8[x9] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(9 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 9 and type Float at device (pre-rename) x39
  NCCLCHECK(ncclAllReduce(x10, x10, (size_t)(9 * sizeof(float)), ncclFloat32, ncclSum, x4, x5));
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(9 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(9 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  int x19 = 0;
  while (x19 != 10) {
    // begin initializing random GPU array of size 0 and type Float at device (pre-rename) x39
    float* x36 = (float*)malloc(0 * sizeof(float));
    int x37 = 0;
    while (x37 != 0) {
      x36[x37] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
      x37 = x37 + 1;
    }
    CUDA_CALL(cudaSetDevice(x6));
    float* x38 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x38, (size_t)0));
    CUDA_CALL(cudaMemcpy(x38, x36, (size_t)0, cudaMemcpyHostToDevice));
    // end initializing random GPU array of size 0 and type Float at device (pre-rename) x39
    // begin initializing fixed GPU array of size 0 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    float* x39 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x39, (size_t)0));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x39, 1, 0);
    // end initializing fixed GPU array of size 0 and type Float and device (pre-rename) x39
    // begin creating and setting tensor descriptor of shape List(1, 1, 9, 9)
    cudnnTensorDescriptor_t x40;
    cudnnCreateTensorDescriptor(&x40);
    cudnnSetTensor4dDescriptor(x40, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, 9, 9);
    // end creating and setting tensor descriptor
    // begin creating and setting filter descriptor of shape List(1, 1, 3, 3)
    cudnnFilterDescriptor_t x41;
    cudnnCreateFilterDescriptor(&x41);
    cudnnSetFilter4dDescriptor(x41, CUDNN_DATA_FLOAT, CUDNN_TENSOR_NCHW, 1, 1, 3, 3);
    // end creating and setting filter descriptor
    // begin creating and setting convolution descriptor of padding: List(1, 1), strides: List(1, 1), dilation: List(1, 1)
    cudnnConvolutionDescriptor_t x42;
    cudnnCreateConvolutionDescriptor(&x42);
    cudnnSetConvolution2dDescriptor(x42, 1, 1, 1, 1, 1, 1, CUDNN_CONVOLUTION, CUDNN_DATA_FLOAT);
    // end creating and setting convolution descriptor
    // begin allocating gpu array for the gradient of filter of convolution
    CUDA_CALL(cudaSetDevice(x6));
    float* x43 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x43, (size_t)(9 * sizeof(float))));
    // end allocating gpu array for the gradient of filter of convolution
    // begin finding convolution backward filter algorithm
    cudnnConvolutionBwdFilterAlgoPerf_t x44;
    int x45 = 0;
    CUDNNCHECK(cudnnFindConvolutionBackwardFilterAlgorithm(x7, x40, x40, x42, x41, 1, &x45, &x44));
    cudnnConvolutionBwdFilterAlgo_t x46 = x44.algo;
    // end finding convolution backward filter algorithm
    // begin finding convolution backward filter workspace size
    size_t x47 = (size_t)0;
    CUDNNCHECK(cudnnGetConvolutionBackwardFilterWorkspaceSize(x7, x40, x40, x42, x41, x46, &x47));
    // end finding convolution backward filter workspace size
    // begin allocating gpu array for onvolution backward filter workspace
    CUDA_CALL(cudaSetDevice(x6));
    float* x48 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x48, (size_t)(x47 * sizeof(float))));
    // end allocating gpu array for onvolution backward filter workspace
    // begin convolution backward filter pass
    float x49 = 1.0;
    float x50 = 0.0;
    CUDNNCHECK(cudnnConvolutionBackwardFilter(x7, &x49, x40, x38, x40, x39, x42, x46, x48, x47, &x50, x41, x43));
    // end convolution backward filter pass
    // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x93 and addition_operand x217
    CUDA_CALL(cudaSetDevice(x6));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x43, 9);
    // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x93 and addition_operand x217
    // begin computing SGD on GPU for size 9 and type Float at device (pre-name) x39 with weight x70, grad x93, and momentum x131
    CUDA_CALL(cudaSetDevice(x6));
    x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 9);
    // end computing SGD on GPU for size 9 and type Float at device (pre-name) x39 with weight x70, grad x93, and momentum x131
    x19 = x19 + 1;
  }
  if (x6 == 0) {
    // begin copying GPU array x70 to CPU and print for size 9 and type Float
    float* x51 = (float*)malloc(9 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x51, x10, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x52 = 0;
    while (x52 != 9) {
      printf("%f ", x51[x52]);
      x52 = x52 + 1;
    }
    printf("\n");
    // end copying GPU array x70 to CPU and print for size 9 and type Float
  }
  printf("compile");
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