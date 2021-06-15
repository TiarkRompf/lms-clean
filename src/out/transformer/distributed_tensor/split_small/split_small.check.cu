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
__global__ void x11(float* x12, float* x13, float* x14, int x15, int x16) {
  // begin kernel for split2
  int x17 = gridDim.x * blockDim.x;
  int x18 = x15 * x16 * 2;
  int x19 = threadIdx.x + blockIdx.x * blockDim.x;
  int x20 = x16 * 2;
  while (x19 < x18) {
    int x21 = x19;
    int x22 = x21 % x20;
    if (x22 < x16) x13[x21 / x20 * x16 + x22] = x12[x21];
    else x14[x21 / x20 * x16 + x22 - x16] = x12[x21];
    x19 = x19 + x17;
  }
  // end kernel for split2
}
__global__ void x24(float* x25, float x26, int x27) {
  // begin generating kernel function for FILL of type Float
  int x28 = gridDim.x * blockDim.x;
  int x29 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x29 < x27) {
    x25[x29] = x26;
    x29 = x29 + x28;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x34(float* x35, float* x36, float* x37, int x38, int x39) {
  // begin kernel for concat2
  int x40 = gridDim.x * blockDim.x;
  int x41 = x38 * x39 * 2;
  int x42 = threadIdx.x + blockIdx.x * blockDim.x;
  int x43 = x39 * 2;
  while (x42 < x41) {
    int x44 = x42;
    int x45 = x44 % x43;
    if (x45 < x39) x37[x44] = x35[x44 / x43 * x39 + x45];
    else x37[x44] = x36[x44 / x43 * x39 + x45 - x39];
    x42 = x42 + x40;
  }
  // end kernel for concat2
}
__global__ void x46(float* x47, float* x48, int x49) {
  // begin generating kernel function for ACCUM of type Float
  int x50 = gridDim.x * blockDim.x;
  int x51 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x51 < x49) {
    int x52 = x51;
    x47[x52] = x47[x52] + x48[x52];
    x51 = x51 + x50;
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
  // begin initializing GPU array of size 512 and type Float
  float* x7 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(512 * sizeof(float))));
  scan_float_array(x7, 512, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 512 and type Float
  // begin computing Split on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input x45
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(256 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(256 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x8, x9, x10, 16, 16);
  // end computing Split on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input x45
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x23 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x23, (size_t)(512 * sizeof(float))));
  x24<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x23, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x30 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x30, (size_t)(256 * sizeof(float))));
  x24<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x30, 0, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 256 and type Float
  float* x31 = (float*)malloc(256 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x31, x9, (size_t)(256 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x31, 256, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 256 and type Float
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x32 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x32, (size_t)(256 * sizeof(float))));
  x24<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x32, 1, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin computing Concat on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input0 x191 input1 x168
  CUDA_CALL(cudaSetDevice(x6));
  float* x33 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x33, (size_t)(512 * sizeof(float))));
  x34<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x32, x30, x33, 16, 16);
  // end computing Concat on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input0 x191 input1 x168
  // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x128 and addition_operand x204
  CUDA_CALL(cudaSetDevice(x6));
  x46<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x23, x33, 512);
  // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x128 and addition_operand x204
  // begin checking GPU array of size 512 and type Float
  float* x53 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x53, x23, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x53, 512, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 512 and type Float
  NCCLCHECK(ncclCommDestroy(x4));
  MPICHECK(MPI_Finalize());
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
