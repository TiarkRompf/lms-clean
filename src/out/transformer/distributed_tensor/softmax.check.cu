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
__global__ void x13(float* x14, float x15, int x16) {
  int x17 = gridDim.x * blockDim.x;
  int x18 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x18 < x16) {
    x14[x18] = x15;
    x18 = x18 + x17;
  }
}
__global__ void x21(float* x22, float* x23, int x24) {
  // begin generating kernel function for ACCUM of type Float
  int x25 = gridDim.x * blockDim.x;
  int x26 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x26 < x24) {
    int x27 = x26;
    x22[x27] = x22[x27] + x23[x27];
    x26 = x26 + x25;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x28(float* x29, float* x30, float* x31, int x32) {
  // begin generating kernel function for SGD of type Float
  int x33 = gridDim.x * blockDim.x;
  int x34 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x34 < x32) {
    int x35 = x34;
    float x36 = x31[x35] * 0.5 + x30[x35];
    x29[x35] = x29[x35] - x36 * 1.0E-4;
    x31[x35] = x36;
    x34 = x34 + x33;
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
  int x3 = MPI_Comm_size(MPI_COMM_WORLD, &x1);
  MPICHECK(x3);
  MPICHECK(MPI_Barrier(MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(x2));
  ncclUniqueId x4;
  NCCLCHECK(ncclGetUniqueId(&x4));
  MPICHECK(MPI_Bcast(&x4, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, MPI_COMM_WORLD));
  ncclComm_t x5;
  NCCLCHECK(ncclCommInitRank(&x5, x1, x4, x2));
  cudaStream_t x6;
  CUDA_CALL(cudaStreamCreateWithFlags(&x6, cudaStreamNonBlocking));
  int x7 = x2;
  // end setting up the MPI/NCCL environment
  // begin setting up the CUDNN environment
  cudnnHandle_t x8;
  CUDNNCHECK(cudnnCreate(&x8));
  // end setting up the CUDNN environment
  // begin initializing random GPU array of size 9 and type Float at device (pre-rename) x39
  float* x9 = (float*)malloc(9 * sizeof(float));
  int x10 = 0;
  while (x10 != 9) {
    x9[x10] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x10 = x10 + 1;
  }
  CUDA_CALL(cudaSetDevice(x7));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(9 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x11, x9, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 9 and type Float at device (pre-rename) x39
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x12 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x12, (size_t)(9 * sizeof(float))));
  x13<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x12, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(9 * sizeof(float))));
  x13<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  int x20 = 0;
  while (x20 != 10) {
    // begin creating and setting tensor descriptor of shape List(2, 1, 3, 3)
    cudnnTensorDescriptor_t x37;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x37));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x37, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 3, 3));
    // end creating and setting tensor descriptor
    // begin allocating gpu array for the output of softmax
    CUDA_CALL(cudaSetDevice(x7));
    float* x38 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x38, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the output of softmax
    // begin softmax forward pass
    float x39 = 1.0;
    float x40 = 0.0;
    CUDNNCHECK(cudnnSoftmaxForward(x8, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, &x39, x37, &x11, &x40, x37, &x38));
    // end softmax forward pass
    // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x41 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x41, (size_t)(9 * sizeof(float))));
    x13<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x41, 0, 9);
    // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x42 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x42, (size_t)(9 * sizeof(float))));
    x13<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, 1, 9);
    // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x169 and addition_operand x182
    CUDA_CALL(cudaSetDevice(x7));
    x21<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x41, x42, 9);
    // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x169 and addition_operand x182
    // begin allocating gpu array for the gradient of input of softmax
    CUDA_CALL(cudaSetDevice(x7));
    float* x43 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x43, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the gradient of input of softmax
    // begin softmax backward pass
    float x44 = 1.0;
    float x45 = 0.0;
    CUDNNCHECK(cudnnSoftmaxBackward(x8, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, &x44, x37, &x38, x37, &x41, &x45, x37, &x43));
    // end softmax backward pass
    // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x86 and addition_operand x232
    CUDA_CALL(cudaSetDevice(x7));
    x21<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x12, x43, 9);
    // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x86 and addition_operand x232
    // begin computing SGD on GPU for size 9 and type Float at device (pre-name) x39 with weight x70, grad x86, and momentum x124
    CUDA_CALL(cudaSetDevice(x7));
    x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x12, x19, 9);
    // end computing SGD on GPU for size 9 and type Float at device (pre-name) x39 with weight x70, grad x86, and momentum x124
    x20 = x20 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x46 = x7 == 0;
  float* x47 = x46 ? ({
    float* x48 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x48, (size_t)(18 * sizeof(float))));
    x48;
  }) : ({
    float* x49 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x49, (size_t)0));
    x49;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  ncclResult_t x50 = ncclSend(x11, (size_t)18, ncclFloat32, 0, x5, x6);
  NCCLCHECK(x50);
  if (x46) {
    int x51 = x1;
    int x52 = 0;
    while (x52 != x51) {
      int x53 = x52;
      NCCLCHECK(ncclRecv(x47 + x53 * 9, (size_t)18, ncclFloat32, x53, x5, x6));
      x52 = x52 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x46) {
    // begin copying GPU array x316 to CPU and print for size 18 and type Float
    float* x54 = (float*)malloc(18 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x54, x47, (size_t)(18 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x55 = 0;
    while (x55 != 18) {
      printf("%f ", x54[x55]);
      x55 = x55 + 1;
    }
    printf("\n");
    // end copying GPU array x316 to CPU and print for size 18 and type Float
  }
  printf("compile");
  CUDNNCHECK(cudnnDestroy(x8));
  MPICHECK(MPI_Finalize());
  NCCLCHECK(ncclCommDestroy(x5));
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
