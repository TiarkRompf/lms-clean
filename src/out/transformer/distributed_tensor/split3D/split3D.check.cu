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
__global__ void x22(float* x23, float** x24) {
  // This is cuda 2-section split kernel for 3D input at axis 2.
  // It takes a 3D array and splits on the innermost dimension (dim2) into 2 arrays.
  // arg0: input array
  // arg1: array of output arrays
  // call constraint: sum of out(i).size = in.size for i in [0, 2)
  int x25 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x25 < 16384) {
    float x26 = x23[x25];
    int x27 = x25 % 32;
    if (x27 < 16) x24[0][x25 / 32 * 16 + x27] = x26;
    else x24[1][x25 / 32 * 16 + (x27 - 16)] = x26;
  }
}
__global__ void x29(float* x30, float* x31, float* x32, int x33) {
  // begin generating kernel function for MULT of type Float
  int x34 = gridDim.x * blockDim.x;
  int x35 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x35 < x33) {
    int x36 = x35;
    x32[x36] = x30[x36] * x31[x36];
    x35 = x35 + x34;
  }
  // end generating kernel function for MULT of type Float
}
__global__ void x43(float* x44, float* x45, int x46) {
  // begin generating kernel function for ACCUM of type Float
  int x47 = gridDim.x * blockDim.x;
  int x48 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x48 < x46) {
    int x49 = x48;
    x44[x49] = x44[x49] + x45[x49];
    x48 = x48 + x47;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x54(float** x55, float* x56) {
  // this is cuda 2-section concat kernel for 3D inputs at axis 2.
  // It concatenates 2 3D arrays on the innermost dimension (dim2).
  // arg0: array of input input arrays
  // arg1: output array
  // call constraint: in.size = 2
  // call constraint: sum of in(i).size = out.size for i in [0, 2)
  int x57 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x57 < 16384) {
    int x58 = x57 % 32;
    if (x58 < 16) x56[x57] = x55[0][x57 / 32 * 16 + x58];
    else x56[x57] = x55[1][x57 / 32 * 16 + (x58 - 16)];
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
  // begin initializing GPU array of size 8192 and type Float
  float* x7 = (float*)malloc(8192 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(8192 * sizeof(float))));
  scan_float_array(x7, 8192, "golden/weight_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(8192 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 8192 and type Float
  // begin initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(8192 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 8192);
  // end initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 16384 and type Float
  float* x16 = (float*)malloc(16384 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(16384 * sizeof(float))));
  scan_float_array(x16, 16384, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(16384 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 16384 and type Float
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(8192 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(8192 * sizeof(float))));
  float** x20 = (float**)malloc(2 * sizeof(float*));
  x20[0] = x18;
  x20[1] = x19;
  float** x21 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x21, (size_t)(2 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x21, x20, (size_t)(2 * sizeof(float*)), cudaMemcpyHostToDevice));
  x22<<<dim3(32, 1, 1), dim3(512, 1, 1)>>>(x17, x21);
  // begin computing MULT on GPU for size 8192 and type Float at device (pre-rename) x39 with left_operand x119 and right_operand x45
  CUDA_CALL(cudaSetDevice(x6));
  float* x28 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x28, (size_t)(8192 * sizeof(float))));
  x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x8, x28, 8192);
  // end computing MULT on GPU for size 8192 and type Float at device (pre-rename) x39 with left_operand x119 and right_operand x45
  // begin initializing fixed GPU array of size 16384 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x37 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x37, (size_t)(16384 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x37, 0, 16384);
  // end initializing fixed GPU array of size 16384 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x38 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x38, (size_t)(8192 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x38, 0, 8192);
  // end initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x39 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x39, (size_t)(8192 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x39, 0, 8192);
  // end initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 8192 and type Float
  float* x40 = (float*)malloc(8192 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x40, x28, (size_t)(8192 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array(x40, 8192, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 8192 and type Float
  // begin initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x41 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x41, (size_t)(8192 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x41, 1, 8192);
  // end initializing fixed GPU array of size 8192 and type Float and device (pre-rename) x39
  // begin computing MULT on GPU for size 8192 and type Float at device (pre-rename) x39 with left_operand x45 and right_operand x281
  CUDA_CALL(cudaSetDevice(x6));
  float* x42 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x42, (size_t)(8192 * sizeof(float))));
  x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x8, x41, x42, 8192);
  // end computing MULT on GPU for size 8192 and type Float at device (pre-rename) x39 with left_operand x45 and right_operand x281
  // begin computing ACCUM on GPU for size 8192 and type Float at device (pre-rename) x39 with base_operand x245 and addition_operand x294
  CUDA_CALL(cudaSetDevice(x6));
  x43<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x38, x42, 8192);
  // end computing ACCUM on GPU for size 8192 and type Float at device (pre-rename) x39 with base_operand x245 and addition_operand x294
  // begin computing MULT on GPU for size 8192 and type Float at device (pre-rename) x39 with left_operand x119 and right_operand x281
  CUDA_CALL(cudaSetDevice(x6));
  float* x50 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x50, (size_t)(8192 * sizeof(float))));
  x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x41, x50, 8192);
  // end computing MULT on GPU for size 8192 and type Float at device (pre-rename) x39 with left_operand x119 and right_operand x281
  // begin computing ACCUM on GPU for size 8192 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x344
  CUDA_CALL(cudaSetDevice(x6));
  x43<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x50, 8192);
  // end computing ACCUM on GPU for size 8192 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x344
  CUDA_CALL(cudaSetDevice(x6));
  float* x51 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x51, (size_t)(16384 * sizeof(float))));
  float** x52 = (float**)malloc(2 * sizeof(float*));
  x52[0] = x38;
  x52[1] = x39;
  float** x53 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x53, (size_t)(2 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x53, x52, (size_t)(2 * sizeof(float*)), cudaMemcpyHostToDevice));
  x54<<<dim3(32, 1, 1), dim3(512, 1, 1)>>>(x53, x51);
  // begin computing ACCUM on GPU for size 16384 and type Float at device (pre-rename) x39 with base_operand x232 and addition_operand x363
  CUDA_CALL(cudaSetDevice(x6));
  x43<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x37, x51, 16384);
  // end computing ACCUM on GPU for size 16384 and type Float at device (pre-rename) x39 with base_operand x232 and addition_operand x363
  // begin checking GPU array of size 8192 and type Float
  float* x59 = (float*)malloc(8192 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x59, x9, (size_t)(8192 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array(x59, 8192, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 8192 and type Float
  // begin checking GPU array of size 16384 and type Float
  float* x60 = (float*)malloc(16384 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x60, x37, (size_t)(16384 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array(x60, 16384, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 16384 and type Float
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
