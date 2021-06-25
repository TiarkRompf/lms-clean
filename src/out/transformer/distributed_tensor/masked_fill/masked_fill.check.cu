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
__global__ void x21(float* x22, float* x23, int* x24, float x25, int x26, int x27, int x28, int x29, int x30) {
  // this is the cuda masked fill kernel.
  // The kernel takes an N-d input tensor `in` and selects two dimensions `i` and `j` to work with.
  // `ijSwapped` is true if and only if i > j.
  // `dim0_shape` and `dim1_shape` are shapes of dimension `i` and `j` in input tensor, respectively.
  // `dim0_stide` and `dim1_stide` denote the physical distance between two logically contigent elements
  // in `i` and `j` of the input array, respectively.
  // The kernel also takes a 2-d `mask` tensor, of shape (dim0_shape, dim1_shape). This mask tensor
  // contains only zeros and ones. The kernel fills elements of input tensor with `value` where mask is
  // zero and stores the result to `out`.
  int x31 = blockIdx.x * blockDim.x + threadIdx.x;
  int x32 = x31;
  int x33 = blockDim.x * gridDim.x;
  int x34 = x31 / x28;
  int x35 = x34;
  int x36 = x34 * x28;
  int x37 = x31 - x36;
  int x38 = x37 / x29;
  int x39 = x38;
  int x40 = x38 * x29;
  int x41 = x36 + x40 + (x37 - x40);
  while (x41 < x30) {
    x23[x41] = x24[x35 % x26 * x27 + x39 % x27] == 0 ? x22[x41] : x25;
    int x42 = x32 + x33;
    x32 = x42;
    int x43 = x42 / x28;
    x35 = x43;
    int x44 = x43 * x28;
    int x45 = x42 - x44;
    int x46 = x45 / x29;
    x39 = x46;
    int x47 = x46 * x29;
    x41 = x44 + x47 + (x45 - x47);
  }
}
__global__ void x49(float* x50, float* x51, float* x52, int x53) {
  // begin generating kernel function for ADD of type Float
  int x54 = gridDim.x * blockDim.x;
  int x55 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x55 < x53) {
    int x56 = x55;
    x52[x56] = x50[x56] + x51[x56];
    x55 = x55 + x54;
  }
  // end generating kernel function for ADD of type Float
}
__global__ void x61(float* x62, float* x63, int x64) {
  // begin generating kernel function for ACCUM of type Float
  int x65 = gridDim.x * blockDim.x;
  int x66 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x66 < x64) {
    int x67 = x66;
    x62[x67] = x62[x67] + x63[x67];
    x66 = x66 + x65;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x69(float* x70, float* x71, int* x72, int x73, int x74, int x75, int x76, int x77) {
  // this is the cuda masked fill gradient kernel.
  // arg0: gradient of N-d output tensor.
  // arg1: gradient of N-d input tensor.
  // Other parameters are same as maskedFill
  int x78 = blockIdx.x * blockDim.x + threadIdx.x;
  int x79 = x78;
  int x80 = blockDim.x * gridDim.x;
  int x81 = x78 / x75;
  int x82 = x81;
  int x83 = x81 * x75;
  int x84 = x78 - x83;
  int x85 = x84 / x76;
  int x86 = x85;
  int x87 = x85 * x76;
  int x88 = x83 + x87 + (x84 - x87);
  while (x88 < x77) {
    if (x72[x82 % x73 * x74 + x86 % x74] == 0) x71[x88] = x71[x88] + x70[x88];
    int x89 = x79 + x80;
    x79 = x89;
    int x90 = x89 / x75;
    x82 = x90;
    int x91 = x90 * x75;
    int x92 = x89 - x91;
    int x93 = x92 / x76;
    x86 = x93;
    int x94 = x93 * x76;
    x88 = x91 + x94 + (x92 - x94);
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
  // begin initializing GPU array of size 81 and type Float
  float* x7 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(81 * sizeof(float))));
  scan_float_array(x7, 81, "golden/weight_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 81 and type Float
  float* x16 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(81 * sizeof(float))));
  scan_float_array(x16, 81, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float
  // begin initializing GPU array of size 81 and type Int
  int* x18 = (int*)malloc(81 * sizeof(int));
  CUDA_CALL(cudaSetDevice(x6));
  int* x19 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(81 * sizeof(int))));
  scan_int_array(x18, 81, "golden/mask_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x19, x18, (size_t)(81 * sizeof(int)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Int
  // begin allocating gpu array of size 81 and type Float for the output of maskedfill
  CUDA_CALL(cudaSetDevice(x6));
  float* x20 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x20, (size_t)(81 * sizeof(float))));
  // end allocating gpu array of size 81 and type Float for the output of maskedfill
  // begin calling masked fill kernel
  x21<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x8, x20, x19, 1.0, 9, 9, 9, 1, 81);
  // end calling masked fill kernel
  // begin computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x138
  CUDA_CALL(cudaSetDevice(x6));
  float* x48 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x48, (size_t)(81 * sizeof(float))));
  x49<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x20, x48, 81);
  // end computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x138
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x57 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x57, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x57, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x58 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x58, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 81 and type Float
  float* x59 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x59, x48, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x59, 81, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 81 and type Float
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x60 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x60, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x60, 1, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x286 and addition_operand x309
  CUDA_CALL(cudaSetDevice(x6));
  x61<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, x60, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x286 and addition_operand x309
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x273 and addition_operand x309
  CUDA_CALL(cudaSetDevice(x6));
  x61<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x57, x60, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x273 and addition_operand x309
  // begin allocating gpu array of size 81 and type Float for the gradient input of masked fill
  CUDA_CALL(cudaSetDevice(x6));
  float* x68 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x68, (size_t)(81 * sizeof(float))));
  // end allocating gpu array of size 81 and type Float for the gradient input of masked fill
  // begin calling masked fill gradient kernel
  x69<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x58, x68, x19, 9, 9, 9, 1, 81);
  // end calling masked fill gradient kernel
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x366
  CUDA_CALL(cudaSetDevice(x6));
  x61<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x68, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x366
  // begin checking GPU array of size 81 and type Float
  float* x95 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x95, x9, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x95, 81, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 81 and type Float
  // begin checking GPU array of size 81 and type Float
  float* x96 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x96, x57, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x96, 81, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 81 and type Float
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
