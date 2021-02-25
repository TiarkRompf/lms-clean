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
    // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x37 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x37, (size_t)(9 * sizeof(float))));
    x13<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x37, 0, 9);
    // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x38 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x38, (size_t)(9 * sizeof(float))));
    x13<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x38, 1, 9);
    // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
    // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x142 and addition_operand x155
    CUDA_CALL(cudaSetDevice(x7));
    x21<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x37, x38, 9);
    // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x142 and addition_operand x155
    // begin creating and setting tensor descriptor of shape List(2, 1, 3, 3)
    cudnnTensorDescriptor_t x39;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x39));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x39, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 3, 3));
    // end creating and setting tensor descriptor
    // begin allocating gpu array for the gradient of input of softmax
    CUDA_CALL(cudaSetDevice(x7));
    float* x40 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x40, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the gradient of input of softmax
    // begin softmax backward pass
    float x41 = 1.0;
    float x42 = 0.0;
    CUDNNCHECK(cudnnSoftmaxBackward(x8, CUDNN_SOFTMAX_FAST, CUDNN_SOFTMAX_MODE_INSTANCE, &x41, &x39, &x11, x39, &x37, &x42, x39, &x40));
    // end softmax backward pass
    // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x86 and addition_operand x214
    CUDA_CALL(cudaSetDevice(x7));
    x21<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x12, x40, 9);
    // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x86 and addition_operand x214
    // begin computing SGD on GPU for size 9 and type Float at device (pre-name) x39 with weight x70, grad x86, and momentum x124
    CUDA_CALL(cudaSetDevice(x7));
    x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x12, x19, 9);
    // end computing SGD on GPU for size 9 and type Float at device (pre-name) x39 with weight x70, grad x86, and momentum x124
    x20 = x20 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x43 = x7 == 0;
  float* x44 = x43 ? ({
    float* x45 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x45, (size_t)(18 * sizeof(float))));
    x45;
  }) : ({
    float* x46 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x46, (size_t)0));
    x46;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  ncclResult_t x47 = ncclSend(x11, (size_t)18, ncclFloat32, 0, x5, x6);
  NCCLCHECK(x47);
  if (x43) {
    int x48 = x1;
    int x49 = 0;
    while (x49 != x48) {
      int x50 = x49;
      NCCLCHECK(ncclRecv(x44 + x50 * 9, (size_t)18, ncclFloat32, x50, x5, x6));
      x49 = x49 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x43) {
    // begin copying GPU array x298 to CPU and print for size 18 and type Float
    float* x51 = (float*)malloc(18 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x51, x44, (size_t)(18 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x52 = 0;
    while (x52 != 18) {
      printf("%f ", x51[x52]);
      x52 = x52 + 1;
    }
    printf("\n");
    // end copying GPU array x298 to CPU and print for size 18 and type Float
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
