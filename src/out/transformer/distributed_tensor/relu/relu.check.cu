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
__global__ void x10(float* x11, float* x12, int x13) {
  // begin generating kernel function for RELU of type Float
  int x14 = gridDim.x * blockDim.x;
  int x15 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x15 < x13) {
    int x16 = x15;
    x12[x16] = max(0.0, x11[x16]);
    x15 = x15 + x14;
  }
  // end generating kernel function for RELU of type Float
}
__global__ void x18(float* x19, float x20, int x21) {
  // begin generating kernel function for FILL of type Float
  int x22 = gridDim.x * blockDim.x;
  int x23 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x23 < x21) {
    x19[x23] = x20;
    x23 = x23 + x22;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x27(float* x28, float* x29, float* x30, int x31) {
  // begin generating kernel function for RELU_GRAD of type Float
  int x32 = gridDim.x * blockDim.x;
  int x33 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x33 < x31) {
    int x34 = x33;
    x30[x34] = x29[x34] > 0.0 ? x28[x34] : 0.0;
    x33 = x33 + x32;
  }
  // end generating kernel function for RELU_GRAD of type Float
}
__global__ void x35(float* x36, float* x37, int x38) {
  // begin generating kernel function for ACCUM of type Float
  int x39 = gridDim.x * blockDim.x;
  int x40 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x40 < x38) {
    int x41 = x40;
    x36[x41] = x36[x41] + x37[x41];
    x40 = x40 + x39;
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
  // begin initializing GPU array of size 512 and type Float at device (pre-rename) x39 from binary file input
  float* x7 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(512 * sizeof(float))));
  scan_float_rank("golden/input", x6, x7, 512);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 512 and type Float at device (pre-rename) x39 from binary file input
  // begin computing RELU on GPU for size 512 and type Float at device (pre-rename) x39 with operand x45
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(512 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x8, x9, 512);
  // end computing RELU on GPU for size 512 and type Float at device (pre-rename) x39 with operand x45
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(512 * sizeof(float))));
  x18<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 512 and type Float at device (pre-name) x39 again binary file loss
  float* x24 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x24, x9, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x24, 512);
  // end checking GPU array of size 512 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x25 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x25, (size_t)(512 * sizeof(float))));
  x18<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x25, 1, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin computing RELU_GRAD on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x154 and right_operand x45
  CUDA_CALL(cudaSetDevice(x6));
  float* x26 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x26, (size_t)(512 * sizeof(float))));
  x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x25, x8, x26, 512);
  // end computing RELU_GRAD on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x154 and right_operand x45
  // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x104 and addition_operand x167
  CUDA_CALL(cudaSetDevice(x6));
  x35<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x26, 512);
  // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x104 and addition_operand x167
  // begin checking GPU array of size 512 and type Float at device (pre-name) x39 again binary file input_grad
  float* x42 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x42, x17, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, x42, 512);
  // end checking GPU array of size 512 and type Float at device (pre-name) x39 again binary file input_grad
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
