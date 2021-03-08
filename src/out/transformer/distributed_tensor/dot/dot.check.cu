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
__global__ void x11(float* x12, float x13, int x14) {
  // begin generating kernel function for FILL of type Float
  int x15 = gridDim.x * blockDim.x;
  int x16 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x16 < x14) {
    x12[x16] = x13;
    x16 = x16 + x15;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x28(float* x29, float* x30, int x31) {
  // begin generating kernel function for ACCUM of type Float
  int x32 = gridDim.x * blockDim.x;
  int x33 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x33 < x31) {
    int x34 = x33;
    x29[x34] = x29[x34] + x30[x34];
    x33 = x33 + x32;
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
  cublasHandle_t x7;
  CUBLAS_CALL(cublasCreate(&x7));
  // begin initializing GPU array of size 208 and type Float at device (pre-rename) x39 from binary file weight
  float* x8 = (float*)malloc(208 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(208 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x8, 208);
  CUDA_CALL(cudaMemcpy(x9, x8, (size_t)(208 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 208 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 208 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(208 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, 0, 208);
  // end initializing fixed GPU array of size 208 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 208 and type Float at device (pre-rename) x39 from binary file input
  float* x17 = (float*)malloc(208 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(208 * sizeof(float))));
  scan_float_rank("golden/input", x6, x17, 208);
  CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(208 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 208 and type Float at device (pre-rename) x39 from binary file input
  // begin computing DOT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x106 and right_operand x48
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(256 * sizeof(float))));
  float x20 = 1.0;
  float x21 = 0.0;
  CUBLAS_CALL(cublasSgemm(x7, CUBLAS_OP_N, CUBLAS_OP_N, 16, 16, 13, &x20, x9, 16, x18, 13, &x21, x19, 16));
  // end computing DOT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x106 and right_operand x48
  // begin initializing fixed GPU array of size 208 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x22 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x22, (size_t)(208 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x22, 0, 208);
  // end initializing fixed GPU array of size 208 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 256 and type Float at device (pre-name) x39 again binary file loss
  float* x23 = (float*)malloc(256 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x23, x19, (size_t)(256 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, (float*)malloc(256 * sizeof(float)), x23, 256);
  // end checking GPU array of size 256 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x24 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x24, (size_t)(256 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x24, 1, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin computing DOT on GPU for size 208 and type Float at device (pre-rename) x39 with left_operand x106 and right_operand x163 with transpose options
  CUDA_CALL(cudaSetDevice(x6));
  float* x25 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x25, (size_t)(208 * sizeof(float))));
  float x26 = 1.0;
  float x27 = 0.0;
  CUBLAS_CALL(cublasSgemm(x7, CUBLAS_OP_N, CUBLAS_OP_T, 16, 13, 16, &x26, x24, 16, x18, 13, &x27, x25, 16));
  // end computing DOT on GPU for size 208 and type Float at device (pre-rename) x39 with left_operand x106 and right_operand x163 with transpose options
  ncclAllReduce(x25, x25, (size_t)208, ncclFloat32, ncclSum, x4, x5);
  CUDA_CALL(cudaStreamSynchronize(x5));
  // begin computing ACCUM on GPU for size 208 and type Float at device (pre-rename) x39 with base_operand x65 and addition_operand x176
  CUDA_CALL(cudaSetDevice(x6));
  x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x25, 208);
  // end computing ACCUM on GPU for size 208 and type Float at device (pre-rename) x39 with base_operand x65 and addition_operand x176
  // begin computing DOT on GPU for size 208 and type Float at device (pre-rename) x39 with left_operand x163 and right_operand x48 with transpose options
  CUDA_CALL(cudaSetDevice(x6));
  float* x35 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x35, (size_t)(208 * sizeof(float))));
  float x36 = 1.0;
  float x37 = 0.0;
  CUBLAS_CALL(cublasSgemm(x7, CUBLAS_OP_T, CUBLAS_OP_N, 13, 16, 16, &x36, x9, 16, x24, 16, &x37, x35, 13));
  // end computing DOT on GPU for size 208 and type Float at device (pre-rename) x39 with left_operand x163 and right_operand x48 with transpose options
  // begin computing ACCUM on GPU for size 208 and type Float at device (pre-rename) x39 with base_operand x139 and addition_operand x235
  CUDA_CALL(cudaSetDevice(x6));
  x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x22, x35, 208);
  // end computing ACCUM on GPU for size 208 and type Float at device (pre-rename) x39 with base_operand x139 and addition_operand x235
  if (x6 == 0) {
    // begin checking GPU array of size 208 and type Float at device (pre-name) x39 again binary file weight_grad
    float* x38 = (float*)malloc(208 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x38, x10, (size_t)(208 * sizeof(float)), cudaMemcpyDeviceToHost));
    check_float_array_rank("golden/weight_grad", x6, (float*)malloc(208 * sizeof(float)), x38, 208);
    // end checking GPU array of size 208 and type Float at device (pre-name) x39 again binary file weight_grad
  }
  // begin checking GPU array of size 208 and type Float at device (pre-name) x39 again binary file input_grad
  float* x39 = (float*)malloc(208 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x39, x22, (size_t)(208 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, (float*)malloc(208 * sizeof(float)), x39, 208);
  // end checking GPU array of size 208 and type Float at device (pre-name) x39 again binary file input_grad
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
