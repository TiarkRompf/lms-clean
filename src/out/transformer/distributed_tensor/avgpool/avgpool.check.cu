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
__global__ void x25(float* x26, float* x27, float* x28, int x29) {
  // begin generating kernel function for ADD of type Float
  int x30 = gridDim.x * blockDim.x;
  int x31 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x31 < x29) {
    int x32 = x31;
    x28[x32] = x26[x32] + x27[x32];
    x31 = x31 + x30;
  }
  // end generating kernel function for ADD of type Float
}
__global__ void x37(float* x38, float* x39, int x40) {
  // begin generating kernel function for ACCUM of type Float
  int x41 = gridDim.x * blockDim.x;
  int x42 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x42 < x40) {
    int x43 = x42;
    x38[x43] = x38[x43] + x39[x43];
    x42 = x42 + x41;
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
  // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file weight
  float* x8 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(81 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x8, 81);
  CUDA_CALL(cudaMemcpy(x9, x8, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(81 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
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
  // begin creating and setting pooling descriptor
  cudnnPoolingDescriptor_t x20;
  CUDNNCHECK(cudnnCreatePoolingDescriptor(&x20));
  CUDNNCHECK(cudnnSetPooling2dDescriptor(x20, CUDNN_POOLING_AVERAGE_COUNT_INCLUDE_PADDING, CUDNN_PROPAGATE_NAN, 3, 3, 1, 1, 1, 1));
  // end creating and setting pooling descriptor
  // begin allocating gpu array for the output of pooling
  CUDA_CALL(cudaSetDevice(x6));
  float* x21 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x21, (size_t)(162 * sizeof(float))));
  // end allocating gpu array for the output of pooling
  // begin pooling forward pass
  float x22 = 1.0;
  float x23 = 0.0;
  CUDNNCHECK(cudnnPoolingForward(x7, x20, &x22, x19, x9, &x23, x19, x21));
  // end pooling forward pass
  // begin computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x143
  CUDA_CALL(cudaSetDevice(x6));
  float* x24 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x24, (size_t)(81 * sizeof(float))));
  x25<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x21, x24, 81);
  // end computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x143
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x33 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x33, (size_t)(81 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x34 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x34, (size_t)(81 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x34, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file loss
  float* x35 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x35, x24, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, (float*)malloc(81 * sizeof(float)), x35, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x36 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x36, (size_t)(81 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, 1, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x216 and addition_operand x240
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x34, x36, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x216 and addition_operand x240
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x203 and addition_operand x240
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, x36, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x203 and addition_operand x240
  // begin allocating gpu array for the gradient of input of pooling
  CUDA_CALL(cudaSetDevice(x6));
  float* x44 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x44, (size_t)(162 * sizeof(float))));
  // end allocating gpu array for the gradient of input of pooling
  // begin pooling backward pass
  float x45 = 1.0;
  float x46 = 0.0;
  CUDNNCHECK(cudnnPoolingBackward(x7, x20, &x45, x19, x21, x19, x34, x19, x9, &x46, x19, x44));
  // end pooling backward pass
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x297
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x44, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x297
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file weight_grad
  float* x47 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x47, x10, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/weight_grad", x6, (float*)malloc(81 * sizeof(float)), x47, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file weight_grad
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file input_grad
  float* x48 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x48, x33, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, (float*)malloc(81 * sizeof(float)), x48, 81);
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
