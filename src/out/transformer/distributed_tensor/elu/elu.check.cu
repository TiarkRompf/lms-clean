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
  // begin initializing GPU array of size 9 and type Float
  float* x8 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(9 * sizeof(float))));
  scan_float_array(x8, 9, "golden/weight_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x9, x8, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 9 and type Float
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 9 and type Float
  float* x17 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(9 * sizeof(float))));
  scan_float_array(x17, 9, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 9 and type Float
  // begin creating and setting tensor descriptor of shape List(1, 1, 3, 3)
  cudnnTensorDescriptor_t x19;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x19));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x19, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 1, 1, 3, 3));
  // end creating and setting tensor descriptor
  // begin creating and setting activation descriptor
  cudnnActivationDescriptor_t x20;
  CUDNNCHECK(cudnnCreateActivationDescriptor(&x20));
  CUDNNCHECK(cudnnSetActivationDescriptor(x20, CUDNN_ACTIVATION_ELU, CUDNN_PROPAGATE_NAN, 1.0));
  // end creating and setting activation descriptor
  // begin allocating gpu array for the output of softmax
  CUDA_CALL(cudaSetDevice(x6));
  float* x21 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x21, (size_t)(9 * sizeof(float))));
  // end allocating gpu array for the output of softmax
  // begin activation forward pass
  float x22 = 1.0;
  float x23 = 0.0;
  CUDNNCHECK(cudnnActivationForward(x7, x20, &x22, x19, x9, &x23, x19, x21));
  // end activation forward pass
  // begin computing ADD on GPU for size 9 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x143
  CUDA_CALL(cudaSetDevice(x6));
  float* x24 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x24, (size_t)(9 * sizeof(float))));
  x25<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x21, x24, 9);
  // end computing ADD on GPU for size 9 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x143
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x33 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x33, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x34 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x34, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x34, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 9 and type Float
  float* x35 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x35, x24, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x35, 9, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 9 and type Float
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x36 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x36, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, 1, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x216 and addition_operand x239
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x34, x36, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x216 and addition_operand x239
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x203 and addition_operand x239
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, x36, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x203 and addition_operand x239
  // begin allocating gpu array for the gradient of input of activation
  CUDA_CALL(cudaSetDevice(x6));
  float* x44 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x44, (size_t)(9 * sizeof(float))));
  // end allocating gpu array for the gradient of input of activation
  // begin activation backward pass
  float x45 = 1.0;
  float x46 = 0.0;
  CUDNNCHECK(cudnnActivationBackward(x7, x20, &x45, x19, x21, x19, x34, x19, x9, &x46, x19, x44));
  // begin activation backward pass
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x296
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x44, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x296
  // begin checking GPU array of size 9 and type Float
  float* x47 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x47, x10, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x47, 9, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 9 and type Float
  // begin checking GPU array of size 9 and type Float
  float* x48 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x48, x33, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x48, 9, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 9 and type Float
  NCCLCHECK(ncclCommDestroy(x4));
  CUDNNCHECK(cudnnDestroy(x7));
  MPICHECK(MPI_Finalize());
  cudnnDestroyTensorDescriptor(x19);
  cudnnDestroyActivationDescriptor(x20);
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
