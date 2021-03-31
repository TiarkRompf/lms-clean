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
__global__ void x10(float* x11, float x12, int x13) {
  // begin generating kernel function for FILL of type Float
  int x14 = gridDim.x * blockDim.x;
  int x15 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x15 < x13) {
    x11[x15] = x12;
    x15 = x15 + x14;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x20(float* x21, float* x22, float* x23, int x24, int x25) {
  // begin kernel for split2
  int x26 = gridDim.x * blockDim.x;
  int x27 = x24 * x25 * 2;
  int x28 = threadIdx.x + blockIdx.x * blockDim.x;
  int x29 = x25 * 2;
  while (x28 < x27) {
    int x30 = x28;
    int x31 = x30 % x29;
    if (x31 < x25) x22[x30 / x29 * x25 + x31] = x21[x30];
    else x23[x30 / x29 * x25 + x31 - x25] = x21[x30];
    x28 = x28 + x26;
  }
  // end kernel for split2
}
__global__ void x33(float* x34, float* x35, float* x36, int x37) {
  // begin generating kernel function for MULT of type Float
  int x38 = gridDim.x * blockDim.x;
  int x39 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x39 < x37) {
    int x40 = x39;
    x36[x40] = x34[x40] * x35[x40];
    x39 = x39 + x38;
  }
  // end generating kernel function for MULT of type Float
}
__global__ void x47(float* x48, float* x49, int x50) {
  // begin generating kernel function for ACCUM of type Float
  int x51 = gridDim.x * blockDim.x;
  int x52 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x52 < x50) {
    int x53 = x52;
    x48[x53] = x48[x53] + x49[x53];
    x52 = x52 + x51;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x56(float* x57, float* x58, float* x59, int x60, int x61) {
  // begin kernel for concat2
  int x62 = gridDim.x * blockDim.x;
  int x63 = x60 * x61;
  int x64 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x64 < x63) {
    int x65 = x64;
    int x66 = x65 % x60;
    if (x66 < x61) x59[x65] = x57[x65 / x60 * x61 + x66];
    else x59[x65] = x58[x65 / x60 * x61 + x66 - x61];
    x64 = x64 + x62;
  }
  // end kernel for concat2
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
  // begin initializing GPU array of size 256 and type Float at device (pre-rename) x39 from binary file weight
  float* x7 = (float*)malloc(256 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(256 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x7, 256);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(256 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 256 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(256 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 512 and type Float at device (pre-rename) x39 from binary file input
  float* x16 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(512 * sizeof(float))));
  scan_float_rank("golden/input", x6, x16, 512);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 512 and type Float at device (pre-rename) x39 from binary file input
  // begin computing Split on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input x103
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(256 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(256 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x18, x19, 16, 16);
  // end computing Split on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input x103
  // begin computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x120 and right_operand x45
  CUDA_CALL(cudaSetDevice(x6));
  float* x32 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x32, (size_t)(256 * sizeof(float))));
  x33<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x8, x32, 256);
  // end computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x120 and right_operand x45
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x41 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x41, (size_t)(512 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x41, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x42 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x42, (size_t)(256 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, 0, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x43 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x43, (size_t)(256 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x43, 0, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 256 and type Float at device (pre-name) x39 again binary file loss
  float* x44 = (float*)malloc(256 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x44, x32, (size_t)(256 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x44, 256);
  // end checking GPU array of size 256 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x45 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x45, (size_t)(256 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x45, 1, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x45 and right_operand x279
  CUDA_CALL(cudaSetDevice(x6));
  float* x46 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x46, (size_t)(256 * sizeof(float))));
  x33<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x8, x45, x46, 256);
  // end computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x45 and right_operand x279
  // begin computing ACCUM on GPU for size 256 and type Float at device (pre-rename) x39 with base_operand x243 and addition_operand x292
  CUDA_CALL(cudaSetDevice(x6));
  x47<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, x46, 256);
  // end computing ACCUM on GPU for size 256 and type Float at device (pre-rename) x39 with base_operand x243 and addition_operand x292
  // begin computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x120 and right_operand x279
  CUDA_CALL(cudaSetDevice(x6));
  float* x54 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x54, (size_t)(256 * sizeof(float))));
  x33<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x45, x54, 256);
  // end computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x120 and right_operand x279
  // begin computing ACCUM on GPU for size 256 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x342
  CUDA_CALL(cudaSetDevice(x6));
  x47<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x54, 256);
  // end computing ACCUM on GPU for size 256 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x342
  // begin computing Concat on GPU for size 32 x 32 and type Float at device (pre-rename) x39 with input0 x243 input1 x256
  CUDA_CALL(cudaSetDevice(x6));
  float* x55 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x55, (size_t)(1024 * sizeof(float))));
  x56<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, x43, x55, 32, 16);
  // end computing Concat on GPU for size 32 x 32 and type Float at device (pre-rename) x39 with input0 x243 input1 x256
  // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x230 and addition_operand x362
  CUDA_CALL(cudaSetDevice(x6));
  x47<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x41, x55, 512);
  // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x230 and addition_operand x362
  // begin checking GPU array of size 256 and type Float at device (pre-name) x39 again binary file weight_grad
  float* x67 = (float*)malloc(256 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x67, x9, (size_t)(256 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/weight_grad", x6, x67, 256);
  // end checking GPU array of size 256 and type Float at device (pre-name) x39 again binary file weight_grad
  // begin checking GPU array of size 512 and type Float at device (pre-name) x39 again binary file input_grad
  float* x68 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x68, x41, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, x68, 512);
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
