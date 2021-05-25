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
__global__ void x22(float* x23, float* x24, int* x25, float x26, int x27, int x28, int x29, int x30, int x31) {
  // this is the cuda masked fill kernel.
  // The kernel takes an N-d input tensor `in` and selects two dimensions `i` and `j` to work with.
  // `ijSwapped` is true if and only if i > j.
  // `dim0_shape` and `dim1_shape` are shapes of dimension `i` and `j` in input tensor, respectively.
  // `dim0_stide` and `dim1_stide` denote the physical distance between two logically contigent elements
  // in `i` and `j` of the input array, respectively.
  // The kernel also takes a 2-d `mask` tensor, of shape (dim0_shape, dim1_shape). This mask tensor
  // contains only zeros and ones. The kernel fills elements of input tensor with `value` where mask is
  // zero and stores the result to `out`.
  int x32 = blockIdx.x * blockDim.x + threadIdx.x;
  int x33 = x32;
  int x34 = blockDim.x * gridDim.x;
  int x35 = x32 / x29;
  int x36 = x35;
  int x37 = x35 * x29;
  int x38 = x32 - x37;
  int x39 = x38 / x30;
  int x40 = x39;
  int x41 = x39 * x30;
  int x42 = x37 + x41 + (x38 - x41);
  while (x42 < x31) {
    x24[x42] = x25[x36 % x27 * x28 + x40 % x28] == 0 ? x23[x42] : x26;
    int x43 = x33 + x34;
    x33 = x43;
    int x44 = x43 / x29;
    x36 = x44;
    int x45 = x44 * x29;
    int x46 = x43 - x45;
    int x47 = x46 / x30;
    x40 = x47;
    int x48 = x47 * x30;
    x42 = x45 + x48 + (x46 - x48);
  }
}
__global__ void x50(float* x51, float* x52, float* x53, int x54) {
  // begin generating kernel function for ADD of type Float
  int x55 = gridDim.x * blockDim.x;
  int x56 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x56 < x54) {
    int x57 = x56;
    x53[x57] = x51[x57] + x52[x57];
    x56 = x56 + x55;
  }
  // end generating kernel function for ADD of type Float
}
__global__ void x62(float* x63, float* x64, int x65) {
  // begin generating kernel function for ACCUM of type Float
  int x66 = gridDim.x * blockDim.x;
  int x67 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x67 < x65) {
    int x68 = x67;
    x63[x68] = x63[x68] + x64[x68];
    x67 = x67 + x66;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x70(float* x71, float* x72, int* x73, int x74, int x75, int x76, int x77, int x78) {
  // this is the cuda masked fill gradient kernel.
  // arg0: gradient of N-d output tensor.
  // arg1: gradient of N-d input tensor.
  // Other parameters are same as maskedFill
  int x79 = blockIdx.x * blockDim.x + threadIdx.x;
  int x80 = x79;
  int x81 = blockDim.x * gridDim.x;
  int x82 = x79 / x76;
  int x83 = x82;
  int x84 = x82 * x76;
  int x85 = x79 - x84;
  int x86 = x85 / x77;
  int x87 = x86;
  int x88 = x86 * x77;
  int x89 = x84 + x88 + (x85 - x88);
  while (x89 < x78) {
    if (x73[x83 % x74 * x75 + x87 % x75] == 0) x72[x89] = x72[x89] + x71[x89];
    int x90 = x80 + x81;
    x80 = x90;
    int x91 = x90 / x76;
    x83 = x91;
    int x92 = x91 * x76;
    int x93 = x90 - x92;
    int x94 = x93 / x77;
    x87 = x94;
    int x95 = x94 * x77;
    x89 = x92 + x95 + (x93 - x95);
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
  // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file weight
  float* x7 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(81 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x7, 81);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x16 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x16, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x16, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
  float* x17 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(81 * sizeof(float))));
  scan_float_rank("golden/input", x6, x17, 81);
  CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
  // begin initializing GPU array of size 81 and type Int at device (pre-rename) x39 from binary file mask
  int* x19 = (int*)malloc(81 * sizeof(int));
  CUDA_CALL(cudaSetDevice(x6));
  int* x20 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x20, (size_t)(81 * sizeof(int))));
  scan_int_rank("golden/mask", x6, x19, 81);
  CUDA_CALL(cudaMemcpy(x20, x19, (size_t)(81 * sizeof(int)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Int at device (pre-rename) x39 from binary file mask
  CUDA_CALL(cudaSetDevice(x6));
  float* x21 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x21, (size_t)(162 * sizeof(float))));
  x22<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x8, x21, x20, 1.0, 9, 9, 9, 1, 162);
  // begin computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x116 and right_operand x150
  CUDA_CALL(cudaSetDevice(x6));
  float* x49 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x49, (size_t)(81 * sizeof(float))));
  x50<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x21, x49, 81);
  // end computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x116 and right_operand x150
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x58 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x58, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x59 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x59, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x59, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file loss
  float* x60 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x60, x49, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x60, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x61 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x61, (size_t)(81 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x61, 1, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x295 and addition_operand x318
  CUDA_CALL(cudaSetDevice(x6));
  x62<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x59, x61, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x295 and addition_operand x318
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x282 and addition_operand x318
  CUDA_CALL(cudaSetDevice(x6));
  x62<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x58, x61, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x282 and addition_operand x318
  CUDA_CALL(cudaSetDevice(x6));
  float* x69 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x69, (size_t)(162 * sizeof(float))));
  x70<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x59, x69, x16, 9, 9, 9, 1, 162);
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x374
  CUDA_CALL(cudaSetDevice(x6));
  x62<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x69, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x374
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file weight_grad
  float* x96 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x96, x9, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/weight_grad", x6, x96, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file weight_grad
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file input_grad
  float* x97 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x97, x58, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, x97, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file input_grad
  MPICHECK(MPI_Finalize());
  NCCLCHECK(ncclCommDestroy(x4));
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
