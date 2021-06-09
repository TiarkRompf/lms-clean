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
__global__ void x11(float* x12, float* x13, int x14, int x15, int x16) {
  int x17 = blockIdx.z;
  // this is the permutation kernel for List(2, 0, 1)
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimX x dimZ x dimY)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, (dimY+31)/32, dimZ), dim3(32, 8, 1)>>>
  __shared__ float x18[1056];
  // read data from input array to shared memory
  int x19 = 0;
  int x20 = blockIdx.y * 32;
  int x21 = blockIdx.x * 32;
  while (x19 < 32) {
    int x22 = x19;
    int x23 = x17 + threadIdx.z;
    if (x23 < x14 && x20 + (threadIdx.y + x22) < x15 && x21 + threadIdx.x < x16) x18[1056 * threadIdx.z + 33 * (threadIdx.y + x22) + threadIdx.x] = x12[(x20 + (threadIdx.y + x22) + x23 * x15) * x16 + (x21 + threadIdx.x)];
    x19 = x19 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x24 = 0;
  while (x24 < 32) {
    int x25 = x24;
    int x26 = x21 + (threadIdx.y + x25);
    if (x26 < x16 && x17 + threadIdx.z < x14 && x20 + threadIdx.x < x15) x13[(x17 + threadIdx.z + x26 * x14) * x15 + (x20 + threadIdx.x)] = x18[1056 * threadIdx.z + 33 * threadIdx.x + (threadIdx.y + x25)];
    x24 = x24 + 8;
  }
}
__global__ void x29(float* x30, float x31, int x32) {
  // begin generating kernel function for FILL of type Float
  int x33 = gridDim.x * blockDim.x;
  int x34 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x34 < x32) {
    x30[x34] = x31;
    x34 = x34 + x33;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x38(float* x39, float* x40, int x41) {
  // begin generating kernel function for ACCUM of type Float
  int x42 = gridDim.x * blockDim.x;
  int x43 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x43 < x41) {
    int x44 = x43;
    x39[x44] = x39[x44] + x40[x44];
    x43 = x43 + x42;
  }
  // end generating kernel function for ACCUM of type Float
}
/**************** Snippet ****************/
void Snippet(int x0) {
  // begin setting up the MPI/NCCL environment
  int x1 = 0;
  int x2 = 0;
  lms.transformation.tensor.fixedsizedistributedtensortest$compilercdistributedtensor$$anon$5@5859620b.type#lms.thirdparty.mpiops$cnull x3 = NULL;
  MPICHECK(MPI_Init(NULL, x3));
  MPICHECK(MPI_Comm_rank(MPI_COMM_WORLD, &x2));
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x1));
  MPICHECK(MPI_Barrier(MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(x2));
  ncclUniqueId x4;
  NCCLCHECK(ncclGetUniqueId(&x4));
  MPICHECK(MPI_Bcast(&x4, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, MPI_COMM_WORLD));
  ncclComm_t x5;
  NCCLCHECK(ncclCommInitRank(&x5, x1, x4, x2));
  cudaStream_t x6;
  CUDA_CALL(cudaStreamCreateWithFlags(&x6, cudaStreamNonBlocking));
  int x7 = x2;
  // end setting up the MPI/NCCL environment
  // begin initializing GPU array of size 162 and type Float at device (pre-rename) x39 from binary file input
  float* x8 = (float*)malloc(162 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x7));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(162 * sizeof(float))));
  scan_float_rank("golden/input", x7, x8, 162);
  CUDA_CALL(cudaMemcpy(x9, x8, (size_t)(162 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 162 and type Float at device (pre-rename) x39 from binary file input
  CUDA_CALL(cudaSetDevice(x7));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(10 * sizeof(float))));
  x11<<<dim3(0, 1, 1), dim3(0, 1, 1)>>>(x27, x10, 4, 9, 9);
  // begin initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x28 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x28, (size_t)(162 * sizeof(float))));
  x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x28, 0, 162);
  // end initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file loss
  float* x35 = (float*)malloc(162 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x35, x10, (size_t)(162 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x7, x35, 162);
  // end checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x36 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x36, (size_t)(162 * sizeof(float))));
  x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, 1, 162);
  // end initializing fixed GPU array of size 162 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x37 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x37, (size_t)(10 * sizeof(float))));
  x11<<<dim3(0, 1, 1), dim3(0, 1, 1)>>>(x3, x37, 4, 9, 9);
  // begin computing ACCUM on GPU for size 162 and type Float at device (pre-rename) x39 with base_operand x192 and addition_operand x254
  CUDA_CALL(cudaSetDevice(x7));
  x38<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x28, x37, 162);
  // end computing ACCUM on GPU for size 162 and type Float at device (pre-rename) x39 with base_operand x192 and addition_operand x254
  // begin checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file input_grad
  float* x45 = (float*)malloc(162 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x45, x28, (size_t)(162 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x7, x45, 162);
  // end checking GPU array of size 162 and type Float at device (pre-name) x39 again binary file input_grad
  MPICHECK(MPI_Finalize());
  NCCLCHECK(ncclCommDestroy(x5));
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
