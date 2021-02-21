/*****************************************
Emitting C Generated Code
*******************************************/
#include "cudnn_header.h"
#include "nccl_header.h"
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "mpi_header.h"
/************* Functions **************/
__global__ void x12(float* x13, float x14, int x15) {
  int x16 = gridDim.x * blockDim.x;
  int x17 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x17 < x15) {
    x13[x17] = x14;
    x17 = x17 + x16;
  }
}
__global__ void x20(float* x21, float* x22, float* x23, int x24, int x25) {
  // begin kernel for split2
  int x26 = gridDim.x * blockDim.x;
  int x27 = x24 * x25;
  int x28 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x28 < x27) {
    int x29 = x28;
    int x30 = x29 % x24;
    if (x30 < x25) x22[x29 / x24 * x25 + x30] = x21[x29];
    else x23[x29 / x24 * x25 + x30 - x25] = x21[x29];
    x28 = x28 + x26;
  }
  // end kernel for split2
}
__global__ void x31(float* x32, float* x33, float* x34, int x35) {
  // begin generating kernel function for MULT of type Float
  int x36 = gridDim.x * blockDim.x;
  int x37 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x37 < x35) {
    int x38 = x37;
    x34[x38] = x32[x38] * x33[x38];
    x37 = x37 + x36;
  }
  // end generating kernel function for MULT of type Float
}
__global__ void x39(float* x40, float* x41, int x42) {
  // begin generating kernel function for ACCUM of type Float
  int x43 = gridDim.x * blockDim.x;
  int x44 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x44 < x42) {
    int x45 = x44;
    x40[x45] = x40[x45] + x41[x45];
    x44 = x44 + x43;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x46(float* x47, float* x48, float* x49, int x50) {
  // begin generating kernel function for SGD of type Float
  int x51 = gridDim.x * blockDim.x;
  int x52 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x52 < x50) {
    int x53 = x52;
    float x54 = x49[x53] * 0.5 + x48[x53];
    x47[x53] = x47[x53] - x54 * 1.0E-4;
    x49[x53] = x54;
    x52 = x52 + x51;
  }
  // end generating kernel function for SGD of type Float
}
/**************** Snippet ****************/
void Snippet(int x0) {
  // begin setting up the MPI/NCCL environment
  int x1 = 0;
  int x2 = 0;
  MPICHECK(MPI_Init(NULL, NULL));
  MPICHECK(MPI_Comm_rank(MPI_COMM_WORLD, &x2));
  int x3 = MPI_Comm_size(MPI_COMM_WORLD, &x1);
  MPICHECK(x3);
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
  // begin initializing random GPU array of size 256 and type Float at device (pre-rename) x39
  float* x8 = (float*)malloc(256 * sizeof(float));
  int x9 = 0;
  while (x9 != 256) {
    x8[x9] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x7));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(256 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(256 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 256 and type Float at device (pre-rename) x39
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(256 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, 0, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(256 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, 0, 256);
  // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
  int x19 = 0;
  while (x19 != 10) {
    // begin initializing random GPU array of size 512 and type Float at device (pre-rename) x39
    float* x55 = (float*)malloc(512 * sizeof(float));
    int x56 = 0;
    while (x56 != 512) {
      x55[x56] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
      x56 = x56 + 1;
    }
    CUDA_CALL(cudaSetDevice(x7));
    float* x57 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x57, (size_t)(512 * sizeof(float))));
    CUDA_CALL(cudaMemcpy(x57, x55, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
    // end initializing random GPU array of size 512 and type Float at device (pre-rename) x39
    // begin computing Split on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input x158
    CUDA_CALL(cudaSetDevice(x7));
    float* x58 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x58, (size_t)(256 * sizeof(float))));
    CUDA_CALL(cudaSetDevice(x7));
    float* x59 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x59, (size_t)(256 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x57, x58, x59, 16, 16);
    // end computing Split on GPU for size 16 x 32 and type Float at device (pre-rename) x39 with input x158
    // begin initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x60 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x60, (size_t)(256 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x60, 1, 256);
    // end initializing fixed GPU array of size 256 and type Float and device (pre-rename) x39
    // begin computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x174 and right_operand x238
    CUDA_CALL(cudaSetDevice(x7));
    float* x61 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x61, (size_t)(256 * sizeof(float))));
    x31<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, x60, x61, 256);
    // end computing MULT on GPU for size 256 and type Float at device (pre-rename) x39 with left_operand x174 and right_operand x238
    // begin computing ACCUM on GPU for size 256 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x251
    CUDA_CALL(cudaSetDevice(x7));
    x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x61, 256);
    // end computing ACCUM on GPU for size 256 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x251
    // begin computing SGD on GPU for size 256 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    CUDA_CALL(cudaSetDevice(x7));
    x46<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 256);
    // end computing SGD on GPU for size 256 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    x19 = x19 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x62 = x7 == 0;
  float* x63 = x62 ? ({
    float* x64 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x64, (size_t)(512 * sizeof(float))));
    x64;
  }) : ({
    float* x65 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x65, (size_t)0));
    x65;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  ncclResult_t x66 = ncclSend(x10, (size_t)512, ncclFloat32, 0, x5, x6);
  NCCLCHECK(x66);
  if (x62) {
    int x67 = x1;
    int x68 = 0;
    while (x68 != x67) {
      int x69 = x68;
      NCCLCHECK(ncclRecv(x63 + x69 * 256, (size_t)512, ncclFloat32, x69, x5, x6));
      x68 = x68 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x62) {
    // begin copying GPU array x391 to CPU and print for size 512 and type Float
    float* x70 = (float*)malloc(512 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x70, x63, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x71 = 0;
    while (x71 != 512) {
      printf("%f ", x70[x71]);
      x71 = x71 + 1;
    }
    printf("\n");
    // end copying GPU array x391 to CPU and print for size 512 and type Float
  }
  printf("compile\n");
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
