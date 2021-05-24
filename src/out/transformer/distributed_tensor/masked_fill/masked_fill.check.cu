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
__global__ void x10(int* x11, int x12, int x13) {
  // begin generating kernel function for FILL of type Int
  int x14 = gridDim.x * blockDim.x;
  int x15 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x15 < x13) {
    x11[x15] = x12;
    x15 = x15 + x14;
  }
  // end generating kernel function for FILL of type Int
}
__global__ void x20(float* x21, float x22, int x23) {
  // begin generating kernel function for FILL of type Float
  int x24 = gridDim.x * blockDim.x;
  int x25 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x25 < x23) {
    x21[x25] = x22;
    x25 = x25 + x24;
  }
  // end generating kernel function for FILL of type Float
}
__global__ void x29(float* x30, float* x31, int* x32, float x33, int x34, int x35, int x36, int x37, int x38) {
  // this is the cuda masked fill kernel.
  // The kernel takes an N-d input tensor `in` and selects two dimensions `i` and `j` to work with.
  // `ijSwapped` is true if and only if i > j.
  // `dim0_shape` and `dim1_shape` are shapes of dimension `i` and `j` in input tensor, respectively.
  // `dim0_stide` and `dim1_stide` denote the physical distance between two logically contigent elements
  // in `i` and `j` of the input array, respectively.
  // The kernel also takes a 2-d `mask` tensor, of shape (dim0_shape, dim1_shape). This mask tensor
  // contains only zeros and ones. The kernel fills elements of input tensor with `value` where mask is
  // zero and stores the result to `out`.
  int x39 = blockIdx.x * blockDim.x + threadIdx.x;
  int x40 = x39;
  int x41 = blockDim.x * gridDim.x;
  int x42 = x39 / x36;
  int x43 = x42;
  int x44 = x42 * x36;
  int x45 = x39 - x44;
  int x46 = x45 / x37;
  int x47 = x46;
  int x48 = x46 * x37;
  int x49 = x44 + x48 + (x45 - x48);
  while (x49 < x38) {
    x31[x49] = x32[x43 % x34 * x35 + x47 % x35] == 0 ? x30[x49] : x33;
    int x50 = x40 + x41;
    x40 = x50;
    int x51 = x50 / x36;
    x43 = x51;
    int x52 = x51 * x36;
    int x53 = x50 - x52;
    int x54 = x53 / x37;
    x47 = x54;
    int x55 = x54 * x37;
    x49 = x52 + x55 + (x53 - x55);
  }
}
__global__ void x57(float* x58, float* x59, float* x60, int x61) {
  // begin generating kernel function for ADD of type Float
  int x62 = gridDim.x * blockDim.x;
  int x63 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x63 < x61) {
    int x64 = x63;
    x60[x64] = x58[x64] + x59[x64];
    x63 = x63 + x62;
  }
  // end generating kernel function for ADD of type Float
}
__global__ void x69(float* x70, float* x71, int x72) {
  // begin generating kernel function for ACCUM of type Float
  int x73 = gridDim.x * blockDim.x;
  int x74 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x74 < x72) {
    int x75 = x74;
    x70[x75] = x70[x75] + x71[x75];
    x74 = x74 + x73;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x77(float* x78, float* x79, int* x80, int x81, int x82, int x83, int x84, int x85) {
  // this is the cuda masked fill gradient kernel.
  // arg0: gradient of N-d output tensor.
  // arg1: gradient of N-d input tensor.
  // Other parameters are same as maskedFill
  int x86 = blockIdx.x * blockDim.x + threadIdx.x;
  int x87 = x86;
  int x88 = blockDim.x * gridDim.x;
  int x89 = x86 / x83;
  int x90 = x89;
  int x91 = x89 * x83;
  int x92 = x86 - x91;
  int x93 = x92 / x84;
  int x94 = x93;
  int x95 = x93 * x84;
  int x96 = x91 + x95 + (x92 - x95);
  while (x96 < x85) {
    if (x80[x90 % x81 * x82 + x94 % x82] == 0) x79[x96] = x79[x96] + x78[x96];
    int x97 = x87 + x88;
    x87 = x97;
    int x98 = x97 / x83;
    x90 = x98;
    int x99 = x98 * x83;
    int x100 = x97 - x99;
    int x101 = x100 / x84;
    x94 = x101;
    int x102 = x101 * x84;
    x96 = x99 + x102 + (x100 - x102);
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
  // begin initializing GPU array of size 81 and type Int at device (pre-rename) x39 from binary file mask
  int* x7 = (int*)malloc(81 * sizeof(int));
  CUDA_CALL(cudaSetDevice(x6));
  int* x8 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(81 * sizeof(int))));
  scan_int_rank("golden/mask", x6, x7, 81);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(81 * sizeof(int)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Int at device (pre-rename) x39 from binary file mask
  // begin initializing fixed GPU array of size 81 and type Int and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  int* x9 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(81 * sizeof(int))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 81);
  // end initializing fixed GPU array of size 81 and type Int and device (pre-rename) x39
  // begin initializing fixed GPU array of size 81 and type Int and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  int* x16 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x16, (size_t)(81 * sizeof(int))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x16, 0, 81);
  // end initializing fixed GPU array of size 81 and type Int and device (pre-rename) x39
  // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file weight
  float* x17 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(81 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x17, 81);
  CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x19 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x19, (size_t)(81 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
  float* x26 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x27 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x27, (size_t)(81 * sizeof(float))));
  scan_float_rank("golden/input", x6, x26, 81);
  CUDA_CALL(cudaMemcpy(x27, x26, (size_t)(81 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 81 and type Float at device (pre-rename) x39 from binary file input
  CUDA_CALL(cudaSetDevice(x6));
  float* x28 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x28, (size_t)(162 * sizeof(float))));
  x29<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x18, x28, x8, 1.0, 9, 9, 9, 1, 162);
  // begin computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x174 and right_operand x190
  CUDA_CALL(cudaSetDevice(x6));
  float* x56 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x56, (size_t)(81 * sizeof(float))));
  x57<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x27, x28, x56, 81);
  // end computing ADD on GPU for size 81 and type Float at device (pre-rename) x39 with left_operand x174 and right_operand x190
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x65 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x65, (size_t)(81 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x65, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x66 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x66, (size_t)(81 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x66, 0, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file loss
  float* x67 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x67, x56, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x67, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x68 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x68, (size_t)(81 * sizeof(float))));
  x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x68, 1, 81);
  // end initializing fixed GPU array of size 81 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x335 and addition_operand x358
  CUDA_CALL(cudaSetDevice(x6));
  x69<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x66, x68, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x335 and addition_operand x358
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x322 and addition_operand x358
  CUDA_CALL(cudaSetDevice(x6));
  x69<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x65, x68, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x322 and addition_operand x358
  CUDA_CALL(cudaSetDevice(x6));
  float* x76 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x76, (size_t)(162 * sizeof(float))));
  x77<<<dim3(1, 1, 1), dim3(512, 1, 1)>>>(x66, x76, x16, 9, 9, 9, 1, 162);
  // begin computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x133 and addition_operand x414
  CUDA_CALL(cudaSetDevice(x6));
  x69<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, x76, 81);
  // end computing ACCUM on GPU for size 81 and type Float at device (pre-rename) x39 with base_operand x133 and addition_operand x414
  // begin checking GPU array of size 81 and type Int at device (pre-name) x39 again binary file mask_grad
  int* x103 = (int*)malloc(81 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x103, x9, (size_t)(81 * sizeof(int)), cudaMemcpyDeviceToHost));
  check_int_array_rank("golden/mask_grad", x6, x103, 81);
  // end checking GPU array of size 81 and type Int at device (pre-name) x39 again binary file mask_grad
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file weight_grad
  float* x104 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x104, x19, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/weight_grad", x6, x104, 81);
  // end checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file weight_grad
  // begin checking GPU array of size 81 and type Float at device (pre-name) x39 again binary file input_grad
  float* x105 = (float*)malloc(81 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x105, x65, (size_t)(81 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, x105, 81);
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
