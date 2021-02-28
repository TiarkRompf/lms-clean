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
  // begin initializing random GPU array of size 18 and type Float at device (pre-rename) x39
  float* x8 = (float*)malloc(18 * sizeof(float));
  int x9 = 0;
  while (x9 != 18) {
    x8[x9] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(18 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(18 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 18 and type Float at device (pre-rename) x39
  NCCLCHECK(ncclAllReduce(x10, x10, (size_t)(18 * sizeof(float)), ncclFloat32, ncclSum, x4, x5));
  // begin initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(18 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, 0, 18);
  // end initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(18 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, 0, 18);
  // end initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
  int x19 = 0;
  while (x19 != 10) {
    // begin creating and setting tensor descriptor of shape List(2, 1, 3, 3)
    cudnnTensorDescriptor_t x36;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x36));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x36, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 3, 3));
    // end creating and setting tensor descriptor
    // begin finding dropout forward reserve space bytes
    size_t x37 = (size_t)0;
    CUDNNCHECK(cudnnDropoutGetReserveSpaceSize(x36, &x37));
    // end finding dropout forward reserve space bytes
    // begin finding dropout forward states bytes
    size_t x38 = (size_t)0;
    CUDNNCHECK(cudnnDropoutGetStatesSize(x7, &x38));
    // end finding dropout forward states bytes
    // begin allocating gpu array for the reserve space of dropout forward
    CUDA_CALL(cudaSetDevice(x6));
    float* x39 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x39, (size_t)x37));
    // end allocating gpu array for the reserve space of dropout forward
    // begin allocating gpu array for the states of dropout forward
    CUDA_CALL(cudaSetDevice(x6));
    float* x40 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x40, (size_t)x38));
    // end allocating gpu array for the states of dropout forward
    // begin creating dropout descriptor
    cudnnDropoutDescriptor_t x41;
    CUDNNCHECK(cudnnCreateDropoutDescriptor(&x41));
    CUDNNCHECK(cudnnSetDropoutDescriptor(x41, x7, 0.5, x40, x38, 1));
    // end creating dropout descriptor
    // begin allocating gpu array for the output of dropout
    CUDA_CALL(cudaSetDevice(x6));
    float* x42 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x42, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the output of dropout
    // begin dropout forward pass
    CUDNNCHECK(cudnnDropoutForward(x7, x41, x36, x10, x36, x42, x39, x37));
    // end dropout forward pass
    // begin initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    float* x43 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x43, (size_t)(18 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x43, 0, 18);
    // end initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
    // begin allocating gpu array for the gradient of input of dropout
    CUDA_CALL(cudaSetDevice(x6));
    float* x44 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x44, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the gradient of input of dropout
    // begin finding dropout backward reserve bytes
    size_t x45 = (size_t)0;
    CUDNNCHECK(cudnnDropoutGetReserveSpaceSize(x36, &x45));
    // end finding dropout backward reserve bytes
    // begin finding dropout backward states bytes
    size_t x46 = (size_t)0;
    CUDNNCHECK(cudnnDropoutGetStatesSize(x7, &x46));
    // end finding dropout backward states bytes
    // begin allocating gpu array for the states of dropout backward
    CUDA_CALL(cudaSetDevice(x6));
    float* x47 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x47, (size_t)x46));
    // end allocating gpu array for the states of dropout backward
    // begin creating dropout descriptor
    cudnnDropoutDescriptor_t x48;
    CUDNNCHECK(cudnnCreateDropoutDescriptor(&x48));
    CUDNNCHECK(cudnnSetDropoutDescriptor(x48, x7, 0.5, x47, x46, 1));
    // end creating dropout descriptor
    // begin dropout backward pass
    CUDNNCHECK(cudnnDropoutBackward(x7, x48, x36, x43, x36, x44, x39, x45));
    // end dropout backward pass
    // begin computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x93 and addition_operand x223
    CUDA_CALL(cudaSetDevice(x6));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x44, 18);
    // end computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x93 and addition_operand x223
    // begin computing SGD on GPU for size 18 and type Float at device (pre-name) x39 with weight x70, grad x93, and momentum x131
    CUDA_CALL(cudaSetDevice(x6));
    x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 18);
    // end computing SGD on GPU for size 18 and type Float at device (pre-name) x39 with weight x70, grad x93, and momentum x131
    x19 = x19 + 1;
  }
  if (x6 == 0) {
    // begin copying GPU array x70 to CPU and print for size 18 and type Float
    float* x49 = (float*)malloc(18 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x49, x10, (size_t)(18 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x50 = 0;
    while (x50 != 18) {
      printf("%f ", x49[x50]);
      x50 = x50 + 1;
    }
    printf("\n");
    // end copying GPU array x70 to CPU and print for size 18 and type Float
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
