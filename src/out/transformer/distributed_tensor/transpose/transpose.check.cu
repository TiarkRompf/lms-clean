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
__global__ void x10(float* x11, float* x12, int x13, int x14) {
  // Cuda Coalesced Transpose
  // arg0: 2D Input Matrix (n x m)
  // arg1: 2D Output Transposed Matrix (m x n)
  // arg2: number of rows for input matrix
  // arg3: number of columns for input matrix
  // kernel launch config <<dim3((TILE_DIM * m - 1) / TILE_DIM, (TILE_DIM * n - 1) / TILE_DIM), dim3(TILE_DIM, BLOCK_ROWS)>>
  // TILE_DIM = 32, BLOCK_ROWS = 8
  __shared__ float x15[1056];
  int x16 = blockIdx.x * 32 + threadIdx.x;
  int x17 = blockIdx.y * 32 + threadIdx.y;
  int x18 = 0;
  while (x18 < 32) {
    int x19 = x18;
    if (x16 < x14 && x17 < x13) x15[33 * (threadIdx.y + x19) + threadIdx.x] = x11[x17 * x14 + x16];
    x17 = x17 + 8;
    x18 = x18 + 8;
  }
  __syncthreads();
  x16 = blockIdx.y * 32 + threadIdx.x;
  x17 = blockIdx.x * 32 + threadIdx.y;
  int x20 = 0;
  while (x20 < 32) {
    int x21 = x20;
    if (x16 < x13 && x17 < x14) x12[x17 * x13 + x16] = x15[33 * threadIdx.x + (threadIdx.y + x21)];
    x17 = x17 + 8;
    x20 = x20 + 8;
  }
}
__global__ void x23(float* x24, float x25, int x26) {
  // begin generating kernel function for FILL of type Float
  int x27 = gridDim.x * blockDim.x;
  int x28 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x28 < x26) {
    x24[x28] = x25;
    x28 = x28 + x27;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x32(float* x33, float* x34, int x35) {
  // begin generating kernel function for ACCUM of type Float
  int x36 = gridDim.x * blockDim.x;
  int x37 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x37 < x35) {
    int x38 = x37;
    x33[x38] = x33[x38] + x34[x38];
    x37 = x37 + x36;
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
  // begin initializing GPU array of size 5992 and type Float
  float* x7 = (float*)malloc(5992 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(5992 * sizeof(float))));
  scan_float_array(x7, 5992, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(5992 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 5992 and type Float
  // begin allocating gpu array of size 5992 and type Float for the output of transpose
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(5992 * sizeof(float))));
  // end allocating gpu array of size 5992 and type Float for the output of transpose
  // begin calling transpose kernel
  x10<<<dim3(2, 4, 1), dim3(32, 8, 1)>>>(x8, x9, 107, 56);
  // end calling transpose kernel
  // begin initializing fixed GPU array of size 5992 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x22 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x22, (size_t)(5992 * sizeof(float))));
  x23<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x22, 0, 5992);
  // end initializing fixed GPU array of size 5992 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 5992 and type Float
  float* x29 = (float*)malloc(5992 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x29, x9, (size_t)(5992 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x29, 5992, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 5992 and type Float
  // begin initializing fixed GPU array of size 5992 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x30 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x30, (size_t)(5992 * sizeof(float))));
  x23<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x30, 1, 5992);
  // end initializing fixed GPU array of size 5992 and type Float and device (pre-rename) x39
  // begin allocating gpu array of size 5992 and type Float for the output of transpose
  CUDA_CALL(cudaSetDevice(x6));
  float* x31 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x31, (size_t)(5992 * sizeof(float))));
  // end allocating gpu array of size 5992 and type Float for the output of transpose
  // begin calling transpose kernel
  x10<<<dim3(4, 2, 1), dim3(32, 8, 1)>>>(x30, x31, 56, 107);
  // end calling transpose kernel
  // begin computing ACCUM on GPU for size 5992 and type Float at device (pre-rename) x39 with base_operand x182 and addition_operand x245
  CUDA_CALL(cudaSetDevice(x6));
  x32<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x22, x31, 5992);
  // end computing ACCUM on GPU for size 5992 and type Float at device (pre-rename) x39 with base_operand x182 and addition_operand x245
  // begin checking GPU array of size 5992 and type Float
  float* x39 = (float*)malloc(5992 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x39, x22, (size_t)(5992 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x39, 5992, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 5992 and type Float
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
