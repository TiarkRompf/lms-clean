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
__global__ void x20(bool* x21, bool x22, int x23) {
  int x24 = gridDim.x * blockDim.x;
  int x25 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x25 < x23) {
    x21[x25] = x22;
    x25 = x25 + x24;
  }
}
__global__ void x26(float* x27, float* x28, int x29) {
  // begin generating kernel function for ACCUM of type Float
  int x30 = gridDim.x * blockDim.x;
  int x31 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x31 < x29) {
    int x32 = x31;
    x27[x32] = x27[x32] + x28[x32];
    x31 = x31 + x30;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x33(float* x34, float* x35, float* x36, int x37) {
  // begin generating kernel function for SGD of type Float
  int x38 = gridDim.x * blockDim.x;
  int x39 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x39 < x37) {
    int x40 = x39;
    float x41 = x36[x40] * 0.5 + x35[x40];
    x34[x40] = x34[x40] - x41 * 1.0E-4;
    x36[x40] = x41;
    x39 = x39 + x38;
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
    // begin initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    float* x42 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x42, (size_t)(18 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, 0, 18);
    // end initializing fixed GPU array of size 18 and type Float and device (pre-rename) x39
    // begin initializing fixed GPU array of size 18 and type Boolean and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    bool* x43 = (bool*)malloc(0 * sizeof(bool));
    CUDA_CALL(cudaMalloc(&x43, (size_t)(18 * sizeof(bool))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x43, 0, 18);
    // end initializing fixed GPU array of size 18 and type Boolean and device (pre-rename) x39
    // begin initializing fixed GPU array of size 18 and type Boolean and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    bool* x44 = (bool*)malloc(0 * sizeof(bool));
    CUDA_CALL(cudaMalloc(&x44, (size_t)(18 * sizeof(bool))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x44, 0, 18);
    // end initializing fixed GPU array of size 18 and type Boolean and device (pre-rename) x39
    // begin creating and setting tensor descriptor of shape List(2, 1, 3, 3)
    cudnnTensorDescriptor_t x45;
    CUDNNCHECK(cudnnCreateTensorDescriptor(&x45));
    CUDNNCHECK(cudnnSetTensor4dDescriptor(x45, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 3, 3));
    // end creating and setting tensor descriptor
    // begin allocating gpu array for the gradient of input of dropout
    CUDA_CALL(cudaSetDevice(x6));
    float* x46 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x46, (size_t)(18 * sizeof(float))));
    // end allocating gpu array for the gradient of input of dropout
    // begin finding dropout backward reserve bytes
    size_t x47 = (size_t)0;
    CUDNNCHECK(cudnnDropoutGetReserveSpaceSize(x45, &x47));
    // end finding dropout backward reserve bytes
    // begin finding dropout backward states bytes
    size_t x48 = (size_t)0;
    CUDNNCHECK(cudnnDropoutGetStatesSize(x7, &x48));
    // end finding dropout backward states bytes
    // begin creating dropout descriptor
    cudnnDropoutDescriptor_t x49;
    CUDNNCHECK(cudnnCreateDropoutDescriptor(&x49));
    CUDNNCHECK(cudnnSetDropoutDescriptor(x49, x7, 0.5, x43, x48, 1));
    // end creating dropout descriptor
    // begin dropout backward pass
    CUDNNCHECK(cudnnDropoutBackward(x7, x49, x45, x42, x45, x46, x44, x47));
    // end dropout backward pass
    // begin computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x93 and addition_operand x222
    CUDA_CALL(cudaSetDevice(x6));
    x26<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x46, 18);
    // end computing ACCUM on GPU for size 18 and type Float at device (pre-rename) x39 with base_operand x93 and addition_operand x222
    // begin computing SGD on GPU for size 18 and type Float at device (pre-name) x39 with weight x70, grad x93, and momentum x131
    CUDA_CALL(cudaSetDevice(x6));
    x33<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 18);
    // end computing SGD on GPU for size 18 and type Float at device (pre-name) x39 with weight x70, grad x93, and momentum x131
    x19 = x19 + 1;
  }
  if (x6 == 0) {
    // begin copying GPU array x70 to CPU and print for size 18 and type Float
    float* x50 = (float*)malloc(18 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x50, x10, (size_t)(18 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x51 = 0;
    while (x51 != 18) {
      printf("%f ", x50[x51]);
      x51 = x51 + 1;
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
