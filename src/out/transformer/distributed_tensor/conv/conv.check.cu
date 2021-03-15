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
__global__ void x42(float* x43, float* x44, int x45) {
  // begin generating kernel function for ACCUM of type Float
  int x46 = gridDim.x * blockDim.x;
  int x47 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x47 < x45) {
    int x48 = x47;
    x43[x48] = x43[x48] + x44[x48];
    x47 = x47 + x46;
  }
  // end generating kernel function for ACCUM of type Float
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
  // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
  float* x17 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(81 * sizeof(float))));
  scan_float_rank("golden/input", x6, x17, 81);
  CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
  // begin creating and setting tensor descriptor of shape List(2, 1, 9, 9)
  cudnnTensorDescriptor_t x19;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x19));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x19, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 9, 9));
  // end creating and setting tensor descriptor
  // begin creating and setting filter descriptor of shape List(2, 1, 3, 3)
  cudnnFilterDescriptor_t x20;
  CUDNNCHECK(cudnnCreateFilterDescriptor(&x20));
  CUDNNCHECK(cudnnSetFilter4dDescriptor(x20, CUDNN_DATA_FLOAT, CUDNN_TENSOR_NCHW, 2, 1, 3, 3));
  // end creating and setting filter descriptor
  // begin creating and setting convolution descriptor of padding: List(1, 1), strides: List(1, 1), dilation: List(1, 1)
  cudnnConvolutionDescriptor_t x21;
  CUDNNCHECK(cudnnCreateConvolutionDescriptor(&x21));
  CUDNNCHECK(cudnnSetConvolution2dDescriptor(x21, 1, 1, 1, 1, 1, 1, CUDNN_CROSS_CORRELATION, CUDNN_DATA_FLOAT));
  // end creating and setting convolution descriptor
  // begin creating and setting tensor descriptor of shape List(2, 2, 9, 9)
  cudnnTensorDescriptor_t x22;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x22));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x22, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 2, 9, 9));
  // end creating and setting tensor descriptor
  // begin allocating gpu array for the output of convolution
  CUDA_CALL(cudaSetDevice(x6));
  float* x23 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x23, (size_t)(324 * sizeof(float))));
  // end allocating gpu array for the output of convolution
  // begin finding convolution forward algorithm
  cudnnConvolutionFwdAlgoPerf_t x24;
  int x25 = 0;
  CUDNNCHECK(cudnnFindConvolutionForwardAlgorithm(x7, x19, x20, x21, x22, 1, &x25, &x24));
  cudnnConvolutionFwdAlgo_t x26 = x24.algo;
  // end finding convolution forward algorithm
  // begin finding convolution forward workspace size
  size_t x27 = (size_t)0;
  CUDNNCHECK(cudnnGetConvolutionForwardWorkspaceSize(x7, x19, x20, x21, x22, x26, &x27));
  // begin finding convolution backward workspace size
  // begin allocating gpu array for convolution forward workspace
  CUDA_CALL(cudaSetDevice(x6));
  float* x28 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x28, (size_t)x27));
  // end allocating gpu array for convolution forward workspace
  // begin convolution forward pass
  float x29 = 1.0;
  float x30 = 0.0;
  CUDNNCHECK(cudnnConvolutionForward(x7, &x29, x19, x18, x20, x9, x21, x26, x28, x27, &x30, x22, x23));
  // end convolution forward pass
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x31 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x31, (size_t)(81 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x31, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file loss
  float* x32 = (float*)malloc(162 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x32, x23, (size_t)(162 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, (float*)malloc(162 * sizeof(float)), x32, 162);
  // end checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x33 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x33, (size_t)(162 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, 1, 162);
  // end initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  // begin allocating gpu array for the gradient of filter of convolution
  CUDA_CALL(cudaSetDevice(x6));
  float* x34 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x34, (size_t)(18 * sizeof(float))));
  // end allocating gpu array for the gradient of filter of convolution
  // begin finding convolution backward filter algorithm
  cudnnConvolutionBwdFilterAlgoPerf_t x35;
  int x36 = 0;
  CUDNNCHECK(cudnnFindConvolutionBackwardFilterAlgorithm(x7, x19, x22, x21, x20, 1, &x36, &x35));
  cudnnConvolutionBwdFilterAlgo_t x37 = x35.algo;
  // end finding convolution backward filter algorithm
  // begin finding convolution backward filter workspace size
  size_t x38 = (size_t)0;
  CUDNNCHECK(cudnnGetConvolutionBackwardFilterWorkspaceSize(x7, x19, x22, x21, x20, x37, &x38));
  // end finding convolution backward filter workspace size
  // begin allocating gpu array for convolution backward filter workspace
  CUDA_CALL(cudaSetDevice(x6));
  float* x39 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x39, (size_t)x38));
  // end allocating gpu array for convolution backward filter workspace
  // begin convolution backward filter pass
  float x40 = 1.0;
  float x41 = 0.0;
  CUDNNCHECK(cudnnConvolutionBackwardFilter(x7, &x40, x19, x18, x22, x33, x21, x37, x39, x38, &x41, x20, x34));
  // end convolution backward filter pass
  ncclAllReduce(x34, x34, (size_t)18, ncclFloat32, ncclSum, x4, x5);
  CUDA_CALL(cudaStreamSynchronize(x5));
  // begin computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x236
  CUDA_CALL(cudaSetDevice(x6));
  x42<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x34, 18);
  // end computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x236
  // begin allocating gpu array for the gradient of weight of convolution
  CUDA_CALL(cudaSetDevice(x6));
  float* x49 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x49, (size_t)(162 * sizeof(float))));
  // end allocating gpu array for the gradient of weight of convolution
  // begin finding convolution backward data algorithm
  cudnnConvolutionBwdDataAlgoPerf_t x50;
  int x51 = 0;
  CUDNNCHECK(cudnnFindConvolutionBackwardDataAlgorithm(x7, x20, x22, x21, x19, 1, &x51, &x50));
  cudnnConvolutionBwdDataAlgo_t x52 = x50.algo;
  // end finding convolution backward data algorithm
  // begin finding convolution backward data workspace size
  size_t x53 = (size_t)0;
  CUDNNCHECK(cudnnGetConvolutionBackwardDataWorkspaceSize(x7, x20, x22, x21, x19, x52, &x53));
  // end finding convolution backward data workspace size
  // begin allocating gpu array for convolution backward data workspace
  CUDA_CALL(cudaSetDevice(x6));
  float* x54 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x54, (size_t)x53));
  // end allocating gpu array for convolution backward data workspace
  // begin convolution backward data pass
  float x55 = 1.0;
  float x56 = 0.0;
  CUDNNCHECK(cudnnConvolutionBackwardData(x7, &x55, x20, x9, x22, x33, x21, x52, x54, x53, &x56, x19, x49));
  // end convolution backward data pass
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x199 and addition_operand x317
  CUDA_CALL(cudaSetDevice(x6));
  x42<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x31, x49, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x199 and addition_operand x317
  if (x6 == 0) {
    // begin checking GPU array of size 18 and type Float at device (pre-name) x39 again binary file weight_grad
    float* x57 = (float*)malloc(18 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x57, x10, (size_t)(18 * sizeof(float)), cudaMemcpyDeviceToHost));
    check_float_array_rank("golden/weight_grad", x6, (float*)malloc(18 * sizeof(float)), x57, 18);
    // end checking GPU array of size 18 and type Float at device (pre-name) x39 again binary file weight_grad
  }
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file input_grad
  float* x58 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x58, x31, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, (float*)malloc(81 * sizeof(float)), x58, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file input_grad
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
