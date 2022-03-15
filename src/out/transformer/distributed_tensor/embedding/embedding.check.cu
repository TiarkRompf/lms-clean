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
__global__ void x31(int* x32, float* x33, float* x34, int x35, int x36, int x37) {
  // Cuda Embedding Grad
  // arg0: embedding indicies
  // arg1: embedding output gradient
  // arg2: embedding gradient
  // arg3: indicies size
  // arg4: stride size
  // arg5: padding index (-1 if unsure)
  extern __shared__ float x38[];
  float* x39 = x38 + NVIDIA_WARP_SIZE * threadIdx.y;
  int* x40 = (int *)(x38 + NVIDIA_WARP_SIZE * blockDim.y);
  int x41 = threadIdx.x + blockIdx.x * blockDim.x;
  int x42 = blockDim.x * blockDim.y;
  int x43 = 0;
  bool x44 = x41 < x36;
  while (x43 < x35) {
    int x45 = x43;
    int x46 = threadIdx.x + threadIdx.y * blockDim.x;
    int x47 = x45 + x46;
    if (x47 < x35) x40[x46] = x32[x47];
    int x48 = x45 + blockDim.x * blockDim.y;
    int x49 = x48 < x35 ? x48 : x35;
    int x50 = blockDim.y;
    int x51 = x45;
    while (x51 < x49) {
      int x52 = x51;
      __syncthreads();
      int x53 = x52 + threadIdx.y;
      bool x54 = x53 < x35;
      if (x54 && x44 && x40[x53 - x45] != x37) x39[threadIdx.x] = x33[x53 * x36 + x41];
      __syncthreads();
      if (x54 && x40[x53 - x45] != x37) {
        int x55 = x49 - x52;
        int x56 = blockDim.y;
        int x57 = x55 < x56 ? x55 : x56;
        int x58 = x40[x53 - x45];
        int x59 = x58 == x40[x52 - x45 + threadIdx.x] ? 1 : 0;
        if (threadIdx.x >= x57) x59 = 0;
        int x60 = __ballot_sync(-1, x59);
        int x61 = x60;
        x32[x57] = x60;
        int x62 = __ffs(x60) - 1;
        if (threadIdx.y == x62) {
          x61 = x60 ^ 1 << x62;
          while (x61 > 0) {
            int x63 = __ffs(x61) - 1;
            x39[threadIdx.x] = x39[threadIdx.x] + x38[threadIdx.x + NVIDIA_WARP_SIZE * x63];
            x61 = x61 ^ 1 << x63;
          }
          if (x44) {
            int x64 = x58 * x36 + x41;
            x34[x64] = x34[x64] + x39[threadIdx.x];
          }
        }
      }
      x51 = x51 + x50;
    }
    x43 = x43 + x42;
  }
}
__global__ void x65(float* x66, float* x67, int x68) {
  // begin generating kernel function for ACCUM of type Float
  int x69 = gridDim.x * blockDim.x;
  int x70 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x70 < x68) {
    int x71 = x70;
    x66[x71] = x66[x71] + x67[x71];
    x70 = x70 + x69;
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
  // begin initializing GPU array of size 1200 and type Float
  float* x7 = (float*)malloc(1200 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(1200 * sizeof(float))));
  scan_float_array(x7, 1200, "golden/embed_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(1200 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 1200 and type Float
  // begin initializing fixed GPU array of size 1200 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(1200 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 1200);
  // end initializing fixed GPU array of size 1200 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 5 and type Int
  int* x16 = (int*)malloc(5 * sizeof(int));
  CUDA_CALL(cudaSetDevice(x6));
  int* x17 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(5 * sizeof(int))));
  scan_int_array(x16, 5, "golden/indices_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(5 * sizeof(int)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 5 and type Int
  // begin allocating gpu array of size 300 and type Float for the output of embedding
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(300 * sizeof(float))));
  // end allocating gpu array of size 300 and type Float for the output of embedding
  // begin calling embedding kernel
  x19<<<dim3(60, 1, 1), dim3(5, 1, 1)>>>(x8, x17, x18, 60);
  // end calling embedding kernel
  // begin checking GPU array of size 300 and type Float
  float* x28 = (float*)malloc(300 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x28, x18, (size_t)(300 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x28, 300, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 300 and type Float
  // begin initializing fixed GPU array of size 300 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x29 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x29, (size_t)(300 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x29, 1, 300);
  // end initializing fixed GPU array of size 300 and type Float and device (pre-rename) x39
  // begin allocating gpu array of size 1200 and type Float for the gradient input of embedding
  // begin initializing fixed GPU array of size 1200 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x30 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x30, (size_t)(1200 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x30, 0, 1200);
  // end initializing fixed GPU array of size 1200 and type Float and device (pre-rename) x39
  // end allocating gpu array of size 1200 and type Float for the gradient input of embedding
  // begin calling embedding gradient kernel
  x31<<<dim3(2, 1, 1), dim3(32, 32, 1), 1024 * sizeof(int) + 1024 * sizeof(float)>>>(x17, x29, x30, 5, 60, -1);
  // end calling embedding gradient kernel
  CUDA_CALL(cudaStreamSynchronize(0));
  ncclAllReduce(x30, x30, (size_t)1200, ncclFloat32, ncclSum, x4, x5);
  CUDA_CALL(cudaStreamSynchronize(x5));
  // begin computing ACCUM on GPU for size 1200 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x194
  CUDA_CALL(cudaSetDevice(x6));
  x65<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x30, 1200);
  // end computing ACCUM on GPU for size 1200 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x194
  // begin checking GPU array of size 1200 and type Float
  float* x72 = (float*)malloc(1200 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x72, x9, (size_t)(1200 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x72, 1200, "golden/embed_grad_rank_%d.data", x6);
  // end checking GPU array of size 1200 and type Float
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
