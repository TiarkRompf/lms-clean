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
__global__ void x20(float* x21, float* x22, int x23) {
  // begin generating kernel function for ACCUM of type Float
  int x24 = gridDim.x * blockDim.x;
  int x25 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x25 < x23) {
    int x26 = x25;
    x21[x26] = x21[x26] + x22[x26];
    x25 = x25 + x24;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x27(float* x28, float* x29, float* x30, int x31) {
  // begin generating kernel function for INVERT_GRAD of type Float
  int x32 = gridDim.x * blockDim.x;
  int x33 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x33 < x31) {
    int x34 = x33;
    float x35 = x28[x34];
    x30[x34] = 0.0 - x29[x34] / (x35 * x35);
    x33 = x33 + x32;
  }
  // end generating kernel function for INVERT_GRAD of type Float
}
__global__ void x36(float* x37, float* x38, float* x39, int x40) {
  // begin generating kernel function for SGD of type Float
  int x41 = gridDim.x * blockDim.x;
  int x42 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x42 < x40) {
    int x43 = x42;
    float x44 = x39[x43] * 0.5 + x38[x43];
    x37[x43] = x37[x43] - x44 * 1.0E-4;
    x39[x43] = x44;
    x42 = x42 + x41;
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
  // begin initializing random GPU array of size 512 and type Float at device (pre-rename) x39
  float* x8 = (float*)malloc(512 * sizeof(float));
  int x9 = 0;
  while (x9 != 512) {
    x8[x9] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x7));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(512 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 512 and type Float at device (pre-rename) x39
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x11 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x11, (size_t)(512 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x7));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(512 * sizeof(float))));
  x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  int x19 = 0;
  while (x19 != 10) {
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x45 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x45, (size_t)(512 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x45, 0, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    CUDA_CALL(cudaSetDevice(x7));
    float* x46 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x46, (size_t)(512 * sizeof(float))));
    x12<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x46, 1, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
    // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x137 and addition_operand x150
    CUDA_CALL(cudaSetDevice(x7));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x45, x46, 512);
    // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x137 and addition_operand x150
    // begin computing INVERT_GRAD on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x65 and right_operand x137
    CUDA_CALL(cudaSetDevice(x7));
    float* x47 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x47, (size_t)(512 * sizeof(float))));
    x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x45, x47, 512);
    // end computing INVERT_GRAD on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x65 and right_operand x137
    // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x200
    CUDA_CALL(cudaSetDevice(x7));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x47, 512);
    // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x200
    // begin computing SGD on GPU for size 512 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    CUDA_CALL(cudaSetDevice(x7));
    x36<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 512);
    // end computing SGD on GPU for size 512 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    x19 = x19 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x48 = x7 == 0;
  float* x49 = x48 ? ({
    float* x50 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x50, (size_t)(1024 * sizeof(float))));
    x50;
  }) : ({
    float* x51 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x51, (size_t)0));
    x51;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  ncclResult_t x52 = ncclSend(x10, (size_t)1024, ncclFloat32, 0, x5, x6);
  NCCLCHECK(x52);
  if (x48) {
    int x53 = x1;
    int x54 = 0;
    while (x54 != x53) {
      int x55 = x54;
      NCCLCHECK(ncclRecv(x49 + x55 * 512, (size_t)1024, ncclFloat32, x55, x5, x6));
      x54 = x54 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x48) {
    // begin copying GPU array x312 to CPU and print for size 1024 and type Float
    float* x56 = (float*)malloc(1024 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x56, x49, (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x57 = 0;
    while (x57 != 1024) {
      printf("%f ", x56[x57]);
      x57 = x57 + 1;
    }
    printf("\n");
    // end copying GPU array x312 to CPU and print for size 1024 and type Float
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
