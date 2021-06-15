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
__global__ void x19(float* x20, int* x21, float* x22, int x23) {
  // this is cuda embedding kernel.
  // arg0: 2D embedding table: <n_embedding x embed_size>
  // arg1: 1D indices: <indices_size>
  // arg2: 2D output: <indices_size x embed_size>
  // arg3: embed_size
  // invocation assumption: <<<dim3(a,1,1), dim3(indices_size,1,1)>>> where a <= embed_size
  // each thread block handles one embedding vector
  int x24 = blockDim.x;
  int x25 = threadIdx.x;
  int x26 = x21[blockIdx.x] * x23;
  while (x25 < x23) {
    int x27 = x25;
    x22[blockIdx.x * x23 + x27] = x20[x26 + x27];
    x25 = x25 + x24;
  }
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
  // begin initializing GPU array of size 1200 and type Float at device (pre-rename) x39 from binary file embed
  float* x7 = (float*)malloc(1200 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(1200 * sizeof(float))));
  scan_float_rank("golden/embed", x6, x7, 1200);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(1200 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 1200 and type Float at device (pre-rename) x39 from binary file embed
  // begin initializing fixed GPU array of size 1200 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(1200 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 1200);
  // end initializing fixed GPU array of size 1200 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 5 and type Int at device (pre-rename) x39 from binary file indices
  int* x16 = (int*)malloc(5 * sizeof(int));
  CUDA_CALL(cudaSetDevice(x6));
  int* x17 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(5 * sizeof(int))));
  scan_int_rank("golden/indices", x6, x16, 5);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(5 * sizeof(int)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 5 and type Int at device (pre-rename) x39 from binary file indices
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(300 * sizeof(float))));
  x19<<<dim3(60, 1, 1), dim3(5, 1, 1)>>>(x8, x17, x18, 60);
  ncclAllReduce(x18, x18, (size_t)300, ncclFloat32, ncclSum, x4, x5);
  CUDA_CALL(cudaStreamSynchronize(x5));
  // begin checking GPU array of size 300 and type Float at device (pre-name) x39 again binary file loss
  float* x28 = (float*)malloc(300 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x28, x18, (size_t)(300 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x28, 300);
  // end checking GPU array of size 300 and type Float at device (pre-name) x39 again binary file loss
  if (x6 == 0) {
    // begin checking GPU array of size 1200 and type Float at device (pre-name) x39 again binary file embed_grad
    float* x29 = (float*)malloc(1200 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x29, x9, (size_t)(1200 * sizeof(float)), cudaMemcpyDeviceToHost));
    check_float_array_rank("golden/embed_grad", x6, x29, 1200);
    // end checking GPU array of size 1200 and type Float at device (pre-name) x39 again binary file embed_grad
  }
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
