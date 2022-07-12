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
__global__ void x13(float* x14, float** x15) {
  // This is cuda 2-section split kernel for 3D input at axis 2.
  // It takes a 3D array and splits on the innermost dimension (dim2) into 2 arrays.
  // arg0: input array
  // arg1: array of output arrays
  // call constraint: sum of out(i).size = in.size for i in [0, 2)
  int x16 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x16 < 16384) {
    float x17 = x14[x16];
    int x18 = x16 % 32;
    if (x18 < 16) x15[0][x16 / 32 * 16 + x18] = x17;
    else x15[1][x16 / 32 * 16 + (x18 - 16)] = x17;
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
__global__ void x32(float** x33, float* x34) {
  // this is cuda 2-section concat kernel for 3D inputs at axis 2.
  // It concatenates 2 3D arrays on the innermost dimension (dim2).
  // arg0: array of input input arrays
  // arg1: output array
  // call constraint: in.size = 2
  // call constraint: sum of in(i).size = out.size for i in [0, 2)
  int x35 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x35 < 16384) {
    int x36 = x35 % 32;
    if (x36 < 16) x34[x35] = x33[0][x35 / 32 * 16 + x36];
    else x34[x35] = x33[1][x35 / 32 * 16 + (x36 - 16)];
  }
}
__global__ void x37(float* x38, float* x39, int x40) {
  // begin generating kernel function for ACCUM of type Float
  int x41 = gridDim.x * blockDim.x;
  int x42 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x42 < x40) {
    int x43 = x42;
    x38[x43] = x38[x43] + x39[x43];
    x42 = x42 + x41;
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
  // begin initializing GPU array of size 16384 and type Float
  float* x7 = (float*)malloc(16384 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(16384 * sizeof(float))));
  scan_float_array(x7, 16384, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(16384 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 16384 and type Float
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(8192 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(8192 * sizeof(float))));
  float** x11 = (float**)malloc(2 * sizeof(float*));
  x11[0] = x9;
  x11[1] = x10;
  float** x12 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x12, (size_t)(2 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x12, x11, (size_t)(2 * sizeof(float*)), cudaMemcpyHostToDevice));
  x13<<<dim3(32, 1, 1), dim3(512, 1, 1)>>>(x8, x12);
  // begin initializing fixed GPU array of size 16384 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(16384 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, 0, 16384);
  // end initializing fixed GPU array of size 16384 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x26 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x26, (size_t)(8192 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x26, 0, 8192);
  // end initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 8192 and type Float
  float* x27 = (float*)malloc(8192 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x27, x9, (size_t)(8192 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x27, 8192, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 8192 and type Float
  // begin initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x28 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x28, (size_t)(8192 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x28, 1, 8192);
  // end initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x29 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x29, (size_t)(16384 * sizeof(float))));
  float** x30 = (float**)malloc(2 * sizeof(float*));
  x30[0] = x28;
  x30[1] = x26;
  float** x31 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x31, (size_t)(2 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x31, x30, (size_t)(2 * sizeof(float*)), cudaMemcpyHostToDevice));
  x32<<<dim3(32, 1, 1), dim3(512, 1, 1)>>>(x31, x29);
  // begin computing ACCUM on GPU for size 16384 and type Float at device (pre-rename) x39 with base_operand x131 and addition_operand x206
  CUDA_CALL(cudaSetDevice(x6));
  x37<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, x29, 16384);
  // end computing ACCUM on GPU for size 16384 and type Float at device (pre-rename) x39 with base_operand x131 and addition_operand x206
  // begin checking GPU array of size 16384 and type Float
  float* x44 = (float*)malloc(16384 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x44, x19, (size_t)(16384 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x44, 16384, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 16384 and type Float
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
