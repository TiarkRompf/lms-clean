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
  int x16 = blockIdx.z;
  // this is the permutation kernel for List(2, 0, 1)
  // arg0: 3D input tensor (dimZ x dimY x dimX)
  // arg1: 3D output tensor (dimX x dimZ x dimY)
  // arg2: dimZ of input
  // arg3: dimY of input
  // arg4: dimX of input
  // caller must use <<<dim3((dimX+31)/32, (dimY+31)/32, dimZ), dim3(32, 8, 1)>>>
  __shared__ float x17[1056];
  // read data from input array to shared memory
  int x18 = 0;
  int x19 = blockIdx.y * 32;
  int x20 = blockIdx.x * 32;
  while (x18 < 32) {
    int x21 = x18;
    int x22 = x16 + threadIdx.z;
    if (x22 < x13 && x19 + (threadIdx.y + x21) < x14 && x20 + threadIdx.x < x15) x17[1056 * threadIdx.z + 33 * (threadIdx.y + x21) + threadIdx.x] = x11[(x19 + (threadIdx.y + x21) + x22 * x14) * x15 + (x20 + threadIdx.x)];
    x18 = x18 + 8;
  }
  // sync threads
  __syncthreads();
  // write data from shared memory to output array
  int x23 = 0;
  while (x23 < 32) {
    int x24 = x23;
    int x25 = x20 + (threadIdx.y + x24);
    if (x25 < x15 && x16 + threadIdx.z < x13 && x19 + threadIdx.x < x14) x12[(x16 + threadIdx.z + x25 * x13) * x14 + (x19 + threadIdx.x)] = x17[1056 * threadIdx.z + 33 * threadIdx.x + (threadIdx.y + x24)];
    x23 = x23 + 8;
  }
}
__global__ void x27(float* x28, float x29, int x30) {
  // begin generating kernel function for FILL of type Float
  int x31 = gridDim.x * blockDim.x;
  int x32 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x32 < x30) {
    x28[x32] = x29;
    x32 = x32 + x31;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x36(float* x37, float* x38, int x39) {
  // begin generating kernel function for ACCUM of type Float
  int x40 = gridDim.x * blockDim.x;
  int x41 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x41 < x39) {
    int x42 = x41;
    x37[x42] = x37[x42] + x38[x42];
    x41 = x41 + x40;
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
  // begin initializing GPU array of size 165000 and type Float
  float* x7 = (float*)malloc(165000 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(165000 * sizeof(float))));
  scan_float_array(x7, 165000, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(165000 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 165000 and type Float
  // begin allocating gpu array of size 165000 and type Float for the output of permute
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(165000 * sizeof(float))));
  // end allocating gpu array of size 165000 and type Float for the output of permute
  // begin calling permute kernel
  x10<<<dim3(2, 2, 60), dim3(32, 8, 1)>>>(x8, x9, 60, 55, 50);
  // end calling permute kernel
  // begin initializing fixed GPU array of size 165000 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x26 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x26, (size_t)(165000 * sizeof(float))));
  x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x26, 0, 165000);
  // end initializing fixed GPU array of size 165000 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 165000 and type Float
  float* x33 = (float*)malloc(165000 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x33, x9, (size_t)(165000 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x33, 165000, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 165000 and type Float
  // begin initializing fixed GPU array of size 165000 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x34 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x34, (size_t)(165000 * sizeof(float))));
  x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x34, 1, 165000);
  // end initializing fixed GPU array of size 165000 and type Float and device (pre-rename) x39
  // begin allocating gpu array of size 165000 and type Float for the output of permute
  CUDA_CALL(cudaSetDevice(x6));
  float* x35 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x35, (size_t)(165000 * sizeof(float))));
  // end allocating gpu array of size 165000 and type Float for the output of permute
  // begin calling permute kernel
  x10<<<dim3(2, 2, 50), dim3(32, 8, 1)>>>(x34, x35, 50, 60, 55);
  // end calling permute kernel
  // begin computing ACCUM on GPU for size 165000 and type Float at device (pre-rename) x39 with base_operand x199 and addition_operand x262
  CUDA_CALL(cudaSetDevice(x6));
  x36<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x26, x35, 165000);
  // end computing ACCUM on GPU for size 165000 and type Float at device (pre-rename) x39 with base_operand x199 and addition_operand x262
  // begin checking GPU array of size 165000 and type Float
  float* x43 = (float*)malloc(165000 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x43, x26, (size_t)(165000 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x43, 165000, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 165000 and type Float
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
