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
__global__ void x28(float* x29, float* x30, float* x31, int x32) {
  // begin generating kernel function for DIV of type Float
  int x33 = gridDim.x * blockDim.x;
  int x34 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x34 < x32) {
    int x35 = x34;
    x31[x35] = x29[x35] / x30[x35];
    x34 = x34 + x33;
  }
  // end generating kernel function for DIV of type Float
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
__global__ void x43(float* x44, float* x45, float* x46, int x47) {
  // begin generating kernel function for SGD of type Float
  int x48 = gridDim.x * blockDim.x;
  int x49 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x49 < x47) {
    int x50 = x49;
    float x51 = x46[x50] * 0.5 + x45[x50];
    x44[x50] = x44[x50] - x51 * 1.0E-4;
    x46[x50] = x51;
    x49 = x49 + x48;
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
    // begin computing MULT on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x65 and right_operand x65
    CUDA_CALL(cudaSetDevice(x7));
    float* x52 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x52, (size_t)(512 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x10, x52, 512);
    // end computing MULT on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x65 and right_operand x65
    // begin computing DIV on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x81 and right_operand x137
    CUDA_CALL(cudaSetDevice(x7));
    float* x53 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x53, (size_t)(512 * sizeof(float))));
    x28<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x52, x53, 512);
    // end computing DIV on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x81 and right_operand x137
    // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x181
    CUDA_CALL(cudaSetDevice(x7));
    x36<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x11, x53, 512);
    // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x81 and addition_operand x181
    // begin computing SGD on GPU for size 512 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    CUDA_CALL(cudaSetDevice(x7));
    x43<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x11, x18, 512);
    // end computing SGD on GPU for size 512 and type Float at device (pre-name) x39 with weight x65, grad x81, and momentum x119
    x19 = x19 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x54 = x7 == 0;
  float* x55 = x54 ? ({
    float* x56 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x56, (size_t)(1024 * sizeof(float))));
    x56;
  }) : ({
    float* x57 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x57, (size_t)0));
    x57;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  ncclResult_t x58 = ncclSend(x10, (size_t)1024, ncclFloat32, 0, x5, x6);
  NCCLCHECK(x58);
  if (x54) {
    int x59 = x1;
    int x60 = 0;
    while (x60 != x59) {
      int x61 = x60;
      NCCLCHECK(ncclRecv(x55 + x61 * 512, (size_t)1024, ncclFloat32, x61, x5, x6));
      x60 = x60 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x54) {
    // begin copying GPU array x321 to CPU and print for size 1024 and type Float
    float* x62 = (float*)malloc(1024 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x62, x55, (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x63 = 0;
    while (x63 != 1024) {
      printf("%f ", x62[x63]);
      x63 = x63 + 1;
    }
    printf("\n");
    // end copying GPU array x321 to CPU and print for size 1024 and type Float
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
