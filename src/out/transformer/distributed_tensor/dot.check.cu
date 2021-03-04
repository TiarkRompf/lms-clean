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
  cublasHandle_t x7;
  CUBLAS_CALL(cublasCreate(&x7));
  // begin initializing random GPU array of size 1024 and type Float at device (pre-rename) x39
  float* x8 = (float*)malloc(1024 * sizeof(float));
  int x9 = 0;
  while (x9 != 1024) {
    x8[x9] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(1024 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(1024 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 1024 and type Float at device (pre-rename) x39
  NCCLCHECK(ncclAllReduce(x10, x10, (size_t)(1024 * sizeof(float)), ncclFloat32, ncclSum, x4, x5));
  // begin initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(1024 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, 0, 1024);
  // end initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(1024 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, 0, 1024);
  // end initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  int x19 = 0;
  while (x19 != 10) {
    // begin initializing random GPU array of size 512 and type Float at device (pre-rename) x39
    float* x36 = (float*)malloc(512 * sizeof(float));
    int x37 = 0;
    while (x37 != 512) {
      x36[x37] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
      x37 = x37 + 1;
    }
    CUDA_CALL(cudaSetDevice(x6));
    float* x38 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x38, (size_t)(512 * sizeof(float))));
    CUDA_CALL(cudaMemcpy(x38, x36, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
    // end initializing random GPU array of size 512 and type Float at device (pre-rename) x39
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x6));
    float* x39 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x39, (size_t)(512 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x39, 1, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    // begin computing DOT on GPU for size 1024 and type Float at device (pre-rename) x39 with left_operand x168 and right_operand x184 with transpose options
    CUDA_CALL(cudaSetDevice(x6));
    float* x40 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x40, (size_t)(1024 * sizeof(float))));
    float x41 = 1.0;
    float x42 = 0.0;
    CUBLAS_CALL(cublasSgemm(x7, CUBLAS_OP_N, CUBLAS_OP_T, 32, 32, 32, &x41, x38, 32, x39, 32, &x42, x40, 32));
    // end computing DOT on GPU for size 1024 and type Float at device (pre-rename) x39 with left_operand x168 and right_operand x184 with transpose options
    ncclAllReduce(x40, x40, (size_t)1024, ncclFloat32, ncclSum, x4, x5);
    // begin computing ACCUM on GPU for size 1024 and type Float at device (pre-rename) x39 with base_operand x91 and addition_operand x197
    CUDA_CALL(cudaSetDevice(x6));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x40, 1024);
    // end computing ACCUM on GPU for size 1024 and type Float at device (pre-rename) x39 with base_operand x91 and addition_operand x197
    // begin computing SGD on GPU for size 1024 and type Float at device (pre-name) x39 with weight x68, grad x91, and momentum x129
    CUDA_CALL(cudaSetDevice(x6));
    x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 1024);
    // end computing SGD on GPU for size 1024 and type Float at device (pre-name) x39 with weight x68, grad x91, and momentum x129
    x19 = x19 + 1;
  }
  if (x6 == 0) {
    // begin copying GPU array x68 to CPU and print for size 1024 and type Float
    float* x43 = (float*)malloc(1024 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x43, x10, (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x44 = 0;
    while (x44 != 1024) {
      printf("%f ", x43[x44]);
      x44 = x44 + 1;
    }
    printf("\n");
    // end copying GPU array x68 to CPU and print for size 1024 and type Float
  }
  printf("compile\n");
  CUBLAS_CALL(cublasDestroy(x7));
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
