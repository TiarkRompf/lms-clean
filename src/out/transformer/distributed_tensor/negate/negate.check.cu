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
__global__ void x19(float* x20, float* x21, int x22) {
  // begin generating kernel function for NEGATE of type Float
  int x23 = gridDim.x * blockDim.x;
  int x24 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x24 < x22) {
    int x25 = x24;
    x21[x25] = 0.0 - x20[x25];
    x24 = x24 + x23;
  }
  // end generating kernel function for NEGATE of type Float
}
__global__ void x27(float* x28, float* x29, float* x30, int x31) {
  // begin generating kernel function for ADD of type Float
  int x32 = gridDim.x * blockDim.x;
  int x33 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x33 < x31) {
    int x34 = x33;
    x30[x34] = x28[x34] + x29[x34];
    x33 = x33 + x32;
  }
  // end generating kernel function for ADD of type Float
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
  // begin initializing GPU array of size 512 and type Float
  float* x7 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(512 * sizeof(float))));
  scan_float_array(x7, 512, "golden/weight_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 512 and type Float
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(512 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 512 and type Float
  float* x16 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(512 * sizeof(float))));
  scan_float_array(x16, 512, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 512 and type Float
  // begin computing NEG on GPU for size 512 and type Float at device (pre-rename) x39 with operand x45
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(512 * sizeof(float))));
  x19<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x8, x18, 512);
  // end computing NEG on GPU for size 512 and type Float at device (pre-rename) x39 with operand x45
  // begin computing ADD on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x120
  CUDA_CALL(cudaSetDevice(x6));
  float* x26 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x26, (size_t)(512 * sizeof(float))));
  x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x18, x26, 512);
  // end computing ADD on GPU for size 512 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x120
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x35 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x35, (size_t)(512 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x35, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x36 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x36, (size_t)(512 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 512 and type Float
  float* x37 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x37, x26, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array(x37, 512, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 512 and type Float
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x38 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x38, (size_t)(512 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x38, 1, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x219 and addition_operand x242
  CUDA_CALL(cudaSetDevice(x6));
  x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, x38, 512);
  // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x219 and addition_operand x242
  // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x206 and addition_operand x242
  CUDA_CALL(cudaSetDevice(x6));
  x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x35, x38, 512);
  // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x206 and addition_operand x242
  // begin computing NEG on GPU for size 512 and type Float at device (pre-rename) x39 with operand x219
  CUDA_CALL(cudaSetDevice(x6));
  float* x46 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x46, (size_t)(512 * sizeof(float))));
  x19<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, x46, 512);
  // end computing NEG on GPU for size 512 and type Float at device (pre-rename) x39 with operand x219
  // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x299
  CUDA_CALL(cudaSetDevice(x6));
  x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x46, 512);
  // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x299
  // begin checking GPU array of size 512 and type Float
  float* x47 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x47, x9, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array(x47, 512, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 512 and type Float
  // begin checking GPU array of size 512 and type Float
  float* x48 = (float*)malloc(512 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x48, x35, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array(x48, 512, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 512 and type Float
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
