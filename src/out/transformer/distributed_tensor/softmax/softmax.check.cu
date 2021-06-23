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
__global__ void x24(float* x25, float* x26, float* x27, int x28) {
  // begin generating kernel function for ADD of type Float
  int x29 = gridDim.x * blockDim.x;
  int x30 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x30 < x28) {
    int x31 = x30;
    x27[x31] = x25[x31] + x26[x31];
    x30 = x30 + x29;
  }
  // end generating kernel function for ADD of type Float
}
__global__ void x36(float* x37, float* x38, int x39) {
  // begin generating kernel function for ACCUM of type Float
  int x40 = gridDim.x * blockDim.x;
  int x41 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x41 < x39) {
    int x42 = x41;
    x37[x42] = x37[x42] + x38[x42];
    x41 = x41 + x40;
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
  // begin allocating gpu array of size 9 and type Float for the output of softmax
  CUDA_CALL(cudaSetDevice(x6));
  float* x20 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x20, (size_t)(9 * sizeof(float))));
  // end allocating gpu array of size 9 and type Float for the output of softmax
  // begin softmax forward pass
  float x21 = 1.0;
  float x22 = 0.0;
  CUDNNCHECK(cudnnSoftmaxForward(x7, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, &x21, x19, x9, &x22, x19, x20));
  // end softmax forward pass
  // begin computing ADD on GPU for size 9 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x134
  CUDA_CALL(cudaSetDevice(x6));
  float* x23 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x23, (size_t)(9 * sizeof(float))));
  x24<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x20, x23, 9);
  // end computing ADD on GPU for size 9 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x134
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x32 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x32, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x32, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x33 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x33, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 9 and type Float
  float* x34 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x34, x23, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x34, 9, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 9 and type Float
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x35 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x35, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x35, 1, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x209 and addition_operand x232
  CUDA_CALL(cudaSetDevice(x6));
  x36<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x33, x35, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x209 and addition_operand x232
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x196 and addition_operand x232
  CUDA_CALL(cudaSetDevice(x6));
  x36<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x32, x35, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x196 and addition_operand x232
  // begin allocating gpu array of size 9 and type Float for the gradient input of softmax
  CUDA_CALL(cudaSetDevice(x6));
  float* x43 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x43, (size_t)(9 * sizeof(float))));
  // end allocating gpu array of size 9 and type Float for the gradient input of softmax
  // begin softmax backward pass
  float x44 = 1.0;
  float x45 = 0.0;
  CUDNNCHECK(cudnnSoftmaxBackward(x7, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, &x44, x19, x20, x19, x33, &x45, x19, x43));
  // end softmax backward pass
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x289
  CUDA_CALL(cudaSetDevice(x6));
  x36<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x43, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x289
  // begin checking GPU array of size 9 and type Float
  float* x46 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x46, x10, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x46, 9, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 9 and type Float
  // begin checking GPU array of size 9 and type Float
  float* x47 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x47, x32, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x47, 9, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 9 and type Float
  NCCLCHECK(ncclCommDestroy(x4));
  CUDNNCHECK(cudnnDestroy(x7));
  MPICHECK(MPI_Finalize());
  cudnnDestroyTensorDescriptor(x19);
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
