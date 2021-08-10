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
__global__ void x24(float* x25, float** x26) {
  // This is cuda 4-section split kernel for 3D input at axis 2.
  // It takes a 3D array and splits on the innermost dimension (dim2) into 4 arrays.
  // arg0: input array
  // arg1: array of output arrays
  // call constraint: sum of out(i).size = in.size for i in [0, 4)
  int x27 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x27 < 16384) {
    float x28 = x25[x27];
    int x29 = x27 % 32;
    if (x29 < 8) x26[0][x27 / 32 * 8 + x29] = x28;
    else if (x29 < 16) x26[1][x27 / 32 * 8 + (x29 - 8)] = x28;
    else if (x29 < 24) x26[2][x27 / 32 * 8 + (x29 - 16)] = x28;
    else x26[3][x27 / 32 * 8 + (x29 - 24)] = x28;
  }
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
__global__ void x47(float* x48, float* x49, int x50) {
  // begin generating kernel function for ACCUM of type Float
  int x51 = gridDim.x * blockDim.x;
  int x52 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x52 < x50) {
    int x53 = x52;
    x48[x53] = x48[x53] + x49[x53];
    x52 = x52 + x51;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x58(float** x59, float* x60) {
  // this is cuda 4-section concat kernel for 3D inputs at axis 2.
  // It concatenates 4 3D arrays on the innermost dimension (dim2).
  // arg0: array of input input arrays
  // arg1: output array
  // call constraint: in.size = 4
  // call constraint: sum of in(i).size = out.size for i in [0, 4)
  int x61 = blockIdx.x * blockDim.x + threadIdx.x;
  if (x61 < 16384) {
    int x62 = x61 % 32;
    if (x62 < 8) x60[x61] = x59[0][x61 / 32 * 8 + x62];
    else if (x62 < 16) x60[x61] = x59[1][x61 / 32 * 8 + (x62 - 8)];
    else if (x62 < 24) x60[x61] = x59[2][x61 / 32 * 8 + (x62 - 16)];
    else x60[x61] = x59[3][x61 / 32 * 8 + (x62 - 24)];
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
  // begin initializing GPU array of size 16384 and type Float
  float* x7 = (float*)malloc(16384 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(16384 * sizeof(float))));
  scan_float_array(x7, 16384, "golden/weight_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(16384 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 16384 and type Float
  // begin initializing fixed GPU array of size 16384 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(16384 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 16384);
  // end initializing fixed GPU array of size 16384 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 4096 and type Float
  float* x16 = (float*)malloc(4096 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(4096 * sizeof(float))));
  scan_float_array(x16, 4096, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(4096 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 4096 and type Float
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(4096 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(4096 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x20 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x20, (size_t)(4096 * sizeof(float))));
  CUDA_CALL(cudaSetDevice(x6));
  float* x21 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x21, (size_t)(4096 * sizeof(float))));
  float** x22 = (float**)malloc(4 * sizeof(float*));
  x22[0] = x18;
  x22[1] = x19;
  x22[2] = x20;
  x22[3] = x21;
  float** x23 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x23, (size_t)(4 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x23, x22, (size_t)(4 * sizeof(float*)), cudaMemcpyHostToDevice));
  x24<<<dim3(32, 1, 1), dim3(512, 1, 1)>>>(x8, x23);
  // begin computing MULT on GPU for size 4096 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x119
  CUDA_CALL(cudaSetDevice(x6));
  float* x30 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x30, (size_t)(4096 * sizeof(float))));
  x31<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x18, x30, 4096);
  // end computing MULT on GPU for size 4096 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x119
  // begin initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x39 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x39, (size_t)(4096 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x39, 0, 4096);
  // end initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x40 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x40, (size_t)(4096 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x40, 0, 4096);
  // end initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x41 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x41, (size_t)(4096 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x41, 0, 4096);
  // end initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x42 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x42, (size_t)(4096 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x42, 0, 4096);
  // end initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x43 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x43, (size_t)(4096 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x43, 0, 4096);
  // end initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 4096 and type Float
  float* x44 = (float*)malloc(4096 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x44, x30, (size_t)(4096 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x44, 4096, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 4096 and type Float
  // begin initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x45 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x45, (size_t)(4096 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x45, 1, 4096);
  // end initializing fixed GPU array of size 4096 and type Float and device (pre-rename) x39
  // begin computing MULT on GPU for size 4096 and type Float at device (pre-rename) x39 with left_operand x119 and right_operand x341
  CUDA_CALL(cudaSetDevice(x6));
  float* x46 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x46, (size_t)(4096 * sizeof(float))));
  x31<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x45, x46, 4096);
  // end computing MULT on GPU for size 4096 and type Float at device (pre-rename) x39 with left_operand x119 and right_operand x341
  // begin computing ACCUM on GPU for size 4096 and type Float at device (pre-rename) x39 with base_operand x266 and addition_operand x354
  CUDA_CALL(cudaSetDevice(x6));
  x47<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x39, x46, 4096);
  // end computing ACCUM on GPU for size 4096 and type Float at device (pre-rename) x39 with base_operand x266 and addition_operand x354
  // begin computing MULT on GPU for size 4096 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x341
  CUDA_CALL(cudaSetDevice(x6));
  float* x54 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x54, (size_t)(4096 * sizeof(float))));
  x31<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x45, x54, 4096);
  // end computing MULT on GPU for size 4096 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x341
  // begin computing ACCUM on GPU for size 4096 and type Float at device (pre-rename) x39 with base_operand x279 and addition_operand x404
  CUDA_CALL(cudaSetDevice(x6));
  x47<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x40, x54, 4096);
  // end computing ACCUM on GPU for size 4096 and type Float at device (pre-rename) x39 with base_operand x279 and addition_operand x404
  CUDA_CALL(cudaSetDevice(x6));
  float* x55 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x55, (size_t)(16384 * sizeof(float))));
  float** x56 = (float**)malloc(4 * sizeof(float*));
  x56[0] = x40;
  x56[1] = x41;
  x56[2] = x42;
  x56[3] = x43;
  float** x57 = (float**)malloc(0 * sizeof(float*));
  CUDA_CALL(cudaMalloc(&x57, (size_t)(4 * sizeof(float*))));
  CUDA_CALL(cudaMemcpy(x57, x56, (size_t)(4 * sizeof(float*)), cudaMemcpyHostToDevice));
  x58<<<dim3(32, 1, 1), dim3(512, 1, 1)>>>(x57, x55);
  // begin computing ACCUM on GPU for size 16384 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x423
  CUDA_CALL(cudaSetDevice(x6));
  x47<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x55, 16384);
  // end computing ACCUM on GPU for size 16384 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x423
  // begin checking GPU array of size 16384 and type Float
  float* x63 = (float*)malloc(16384 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x63, x9, (size_t)(16384 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x63, 16384, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 16384 and type Float
  // begin checking GPU array of size 4096 and type Float
  float* x64 = (float*)malloc(4096 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x64, x39, (size_t)(4096 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x64, 4096, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 4096 and type Float
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
