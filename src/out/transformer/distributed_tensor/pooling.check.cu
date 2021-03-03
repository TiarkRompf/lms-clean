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
__global__ void x11(float* x12, float x13, int x14) {
  int x15 = gridDim.x * blockDim.x;
  int x16 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x16 < x14) {
    x12[x16] = x13;
    x16 = x16 + x15;
  }
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
  // begin initializing random GPU array of size 162 and type Float at device (pre-rename) x39
  float* x7 = (float*)malloc(162 * sizeof(float));
  int x8 = 0;
  while (x8 != 162) {
    x7[x8] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x8 = x8 + 1;
  }
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(162 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x9, x7, (size_t)(162 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 162 and type Float at device (pre-rename) x39
  NCCLCHECK(ncclAllReduce(x9, x9, (size_t)(162 * sizeof(float)), ncclFloat32, ncclSum, x4, x5));
  // begin initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(162 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, 0, 162);
  // end initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(162 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, 0, 162);
  // end initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  int x18 = 0;
  while (x18 != 10) {
    // begin creating and setting tensor descriptor of shape List(2, 1, 9, 9)
    cudnnTensorDescriptor_t x35;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x35));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x35, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 9, 9));
    // end creating and setting tensor descriptor
    // begin creating and setting tensor descriptor of shape List(2, 1, 10, 10)
    cudnnTensorDescriptor_t x36;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x36));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x36, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 10, 10));
    // end creating and setting tensor descriptor
    // begin creating and setting pooling descriptor
    cudnnPoolingDescriptor_t x37;
    CUDNNCHECK(cudnnCreatePoolingDescriptor(&x37));
    CUDNNCHECK(cudnnSetPooling2dDescriptor(x37, CUDNN_POOLING_MAX, CUDNN_PROPAGATE_NAN, 2, 2, 1, 1, 1, 1));
    // end creating and setting pooling descriptor
    // begin allocating gpu array for the output of pooling
    CUDA_CALL(cudaSetDevice(x6));
    float* x38 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x38, (size_t)(200 * sizeof(float))));
    // end allocating gpu array for the output of pooling
    // begin setting up the CUDNN environment
    cudnnHandle_t x39;
    CUDNNCHECK(cudnnCreate(&x39));
    // end setting up the CUDNN environment
    float x40 = 1.0;
    float x41 = 0.0;
    CUDNNCHECK(cudnnPoolingForward(x39, x37, &x40, x35, x9, &x41, x36, x38));
    // begin initializing fixed GPU array of size 100 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    float* x42 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x42, (size_t)(100 * sizeof(float))));
    x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, 1, 100);
    // end initializing fixed GPU array of size 100 and type Float and device (pre-rename) x39
    // begin allocating gpu array for the gradient of input of pooling
    CUDA_CALL(cudaSetDevice(x6));
    float* x43 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x43, (size_t)(162 * sizeof(float))));
    // end allocating gpu array for the gradient of input of pooling
    float x44 = 1.0;
    float x45 = 0.0;
    CUDNNCHECK(cudnnPoolingBackward(x39, x37, &x44, x36, x38, x36, x42, x35, x9, &x45, x35, x43));
    // begin computing ACCUM on GPU for size 162 and type Float at device (pre-rename) x39 with base_operand x88 and addition_operand x203
    CUDA_CALL(cudaSetDevice(x6));
    x19<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x43, 162);
    // end computing ACCUM on GPU for size 162 and type Float at device (pre-rename) x39 with base_operand x88 and addition_operand x203
    // begin computing SGD on GPU for size 162 and type Float at device (pre-name) x39 with weight x65, grad x88, and momentum x126
    CUDA_CALL(cudaSetDevice(x6));
    x26<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x10, x17, 162);
    // end computing SGD on GPU for size 162 and type Float at device (pre-name) x39 with weight x65, grad x88, and momentum x126
    x18 = x18 + 1;
  }
  if (x6 == 0) {
    // begin copying GPU array x65 to CPU and print for size 162 and type Float
    float* x46 = (float*)malloc(162 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x46, x9, (size_t)(162 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x47 = 0;
    while (x47 != 162) {
      printf("%f ", x46[x47]);
      x47 = x47 + 1;
    }
    printf("\n");
    // end copying GPU array x65 to CPU and print for size 162 and type Float
  }
  printf("compile");
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
