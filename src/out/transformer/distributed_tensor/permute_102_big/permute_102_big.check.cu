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
__global__ void x10(float* x11, float* x12, int x13, int x14, int x15) {
  // this is the permute kernel for [1, 0, 2]
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimY x dimZ x dimX)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3(dimY, dimZ, 1), dim3(A, 1, 1)>>> where A < dimX
  // each threadblock hands one dimX in coalease size of A, then we have dimZ x dimY threadblocks
  // this kernel might be inefficient if the dimX is small. TODO
  int x16 = blockDim.x;
  int x17 = 0;
  while (x17 < x15) {
    int x18 = x17;
    x12[(blockIdx.y + blockIdx.x * x13) * x15 + (threadIdx.x + x18)] = x11[(blockIdx.x + blockIdx.y * x14) * x15 + (threadIdx.x + x18)];
    x17 = x17 + x16;
  }
}
__global__ void x20(float* x21, float x22, int x23) {
  // begin generating kernel function for FILL of type Float
  int x24 = gridDim.x * blockDim.x;
  int x25 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x25 < x23) {
    x21[x25] = x22;
    x25 = x25 + x24;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x29(float* x30, float* x31, int x32) {
  // begin generating kernel function for ACCUM of type Float
  int x33 = gridDim.x * blockDim.x;
  int x34 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x34 < x32) {
    int x35 = x34;
    x30[x35] = x30[x35] + x31[x35];
    x34 = x34 + x33;
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
  // begin initializing GPU array of size 64 and type Float at device (pre-rename) x39 from binary file input
  float* x7 = (float*)malloc(64 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(64 * sizeof(float))));
  scan_float_rank("golden/input", x6, x7, 64);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(64 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 64 and type Float at device (pre-rename) x39 from binary file input
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(64 * sizeof(float))));
  x10<<<dim3(2, 4, 1), dim3(7, 1, 1)>>>(x8, x9, 4, 2, 8);
  // begin initializing fixed GPU array of size 64 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(64 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, 0, 64);
  // end initializing fixed GPU array of size 64 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 64 and type Float at device (pre-name) x39 again binary file loss
  float* x26 = (float*)malloc(64 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x26, x9, (size_t)(64 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x26, 64);
  // end checking GPU array of size 64 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 64 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x27 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x27, (size_t)(64 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x27, 1, 64);
  // end initializing fixed GPU array of size 64 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x28 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x28, (size_t)(64 * sizeof(float))));
  x10<<<dim3(2, 4, 1), dim3(7, 1, 1)>>>(x27, x28, 4, 2, 8);
  // begin computing ACCUM on GPU for size 64 and type Float at device (pre-rename) x39 with base_operand x119 and addition_operand x181
  CUDA_CALL(cudaSetDevice(x6));
  x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, x28, 64);
  // end computing ACCUM on GPU for size 64 and type Float at device (pre-rename) x39 with base_operand x119 and addition_operand x181
  // begin checking GPU array of size 64 and type Float at device (pre-name) x39 again binary file input_grad
  float* x36 = (float*)malloc(64 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x36, x19, (size_t)(64 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, x36, 64);
  // end checking GPU array of size 64 and type Float at device (pre-name) x39 again binary file input_grad
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
