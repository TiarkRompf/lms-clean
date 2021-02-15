/*****************************************
Emitting C Generated Code
*******************************************/
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
__global__ void x20(float* x21, float* x22, float* x23, int x24) {
  // begin generating kernel function for MULT of type Float
  int x25 = gridDim.x * blockDim.x;
  int x26 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x26 < x24) {
    int x27 = x26;
    x23[x27] = x21[x27] * x22[x27];
    x26 = x26 + x25;
  }
  // end generating kernel function for MULT of type Float
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
__global__ void x35(float* x36, float* x37, float* x38, int x39, int x40) {
  // begin kernel for concat2
  int x41 = gridDim.x * blockDim.x;
  int x42 = x39 * x40;
  int x43 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x43 < x42) {
    int x44 = x43;
    int x45 = x44 % x39;
    if (x45 < x40) x38[x44] = x36[x44 / x39 * x40 + x45];
    else x38[x44] = x37[x44 / x39 * x40 + x45 - x40];
    x43 = x43 + x41;
  }
  // end kernel for concat2
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
  // begin initializing random GPU array of size 1024 and type Float at device (pre-rename) x39
  float* x8 = (float*)malloc(1024 * sizeof(float));
  int x9 = 0;
  while (x9 != 1024) {
    x8[x9] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x7));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(1024 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(1024 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 1024 and type Float at device (pre-rename) x39
  // begin initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(1024 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, 0, 1024);
  // end initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(1024 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, 0, 1024);
  // end initializing fixed GPU array of size 1024 and type Float and device (pre-rename) x39
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
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x58 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x58, (size_t)(512 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, 0, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x59 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x59, (size_t)(512 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x59, 0, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x60 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x60, (size_t)(512 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x60, 1, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    // begin computing MULT on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x158 and right_operand x200
    CUDA_CALL(cudaSetDevice(x7));
    float* x61 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x61, (size_t)(512 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x57, x60, x61, 512);
    // end computing MULT on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x158 and right_operand x200
    // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x174 and addition_operand x213
    CUDA_CALL(cudaSetDevice(x7));
    x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, x61, 512);
    // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x174 and addition_operand x213
    // begin computing Concat on GPU for size 32 x 64 and type Float at device (pre-rename) x39 with input0 x174 input1 x187
    CUDA_CALL(cudaSetDevice(x7));
    float* x62 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x62, (size_t)(2048 * sizeof(float))));
    x35<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, x59, x62, 32, 32);
    // end computing Concat on GPU for size 32 x 64 and type Float at device (pre-rename) x39 with input0 x174 input1 x187
    // begin computing ACCUM on GPU for size 1024 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x294
    CUDA_CALL(cudaSetDevice(x7));
    x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x62, 1024);
    // end computing ACCUM on GPU for size 1024 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x294
    // begin computing SGD on GPU for size 1024 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    CUDA_CALL(cudaSetDevice(x7));
    x46<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 1024);
    // end computing SGD on GPU for size 1024 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    x19 = x19 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x63 = x7 == 0;
  float* x64 = x63 ? ({
    float* x65 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x65, (size_t)(2048 * sizeof(float))));
    x65;
  }) : ({
    float* x66 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x66, (size_t)0));
    x66;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  ncclResult_t x67 = ncclSend(x10, (size_t)2048, ncclFloat32, 0, x5, x6);
  NCCLCHECK(x67);
  if (x63) {
    int x68 = x1;
    int x69 = 0;
    while (x69 != x68) {
      int x70 = x69;
      NCCLCHECK(ncclRecv(x64 + x70 * 1024, (size_t)2048, ncclFloat32, x70, x5, x6));
      x69 = x69 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x63) {
    // begin copying GPU array x415 to CPU and print for size 2048 and type Float
    float* x71 = (float*)malloc(2048 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x71, x64, (size_t)(2048 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x72 = 0;
    while (x72 != 2048) {
      printf("%f ", x71[x72]);
      x72 = x72 + 1;
    }
    printf("\n");
    // end copying GPU array x415 to CPU and print for size 2048 and type Float
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
