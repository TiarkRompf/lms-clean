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
__global__ void x11(float* x12, float x13, int x14) {
  // begin generating kernel function for FILL of type Float
  int x15 = gridDim.x * blockDim.x;
  int x16 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x16 < x14) {
    x12[x16] = x13;
    x16 = x16 + x15;
  }
  // end generating kernel function for FILL of type Float
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
  // begin setting up the CUDNN environment
  cudnnHandle_t x7;
  CUDNNCHECK(cudnnCreate(&x7));
  // end setting up the CUDNN environment
  // begin initializing GPU array of size 9 and type Float at device (pre-rename) x39 from binary file weight
  float* x8 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(9 * sizeof(float))));
  scan_float_rank("golden/weight", x6, x8, 9);
  CUDA_CALL(cudaMemcpy(x9, x8, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 9 and type Float at device (pre-rename) x39 from binary file weight
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 9 and type Float at device (pre-rename) x39 from binary file input
  float* x17 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(9 * sizeof(float))));
  scan_float_rank("golden/input", x6, x17, 9);
  CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(9 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 9 and type Float at device (pre-rename) x39 from binary file input
  // begin creating and setting tensor descriptor of shape List(2, 1, 3, 3)
  cudnnTensorDescriptor_t x19;
  CUDNNCHECK(cudnnCreateTensorDescriptor(&x19));
  CUDNNCHECK(cudnnSetTensor4dDescriptor(x19, CUDNN_TENSOR_NCHW, CUDNN_DATA_FLOAT, 2, 1, 3, 3));
  // end creating and setting tensor descriptor
  // begin finding dropout forward reserve space bytes
  size_t x20 = (size_t)0;
  CUDNNCHECK(cudnnDropoutGetReserveSpaceSize(x19, &x20));
  // end finding dropout forward reserve space bytes
  // begin finding dropout forward states bytes
  size_t x21 = (size_t)0;
  CUDNNCHECK(cudnnDropoutGetStatesSize(x7, &x21));
  // end finding dropout forward states bytes
  // begin allocating gpu array for the reserve space of dropout forward
  CUDA_CALL(cudaSetDevice(x6));
  float* x22 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x22, (size_t)x20));
  // end allocating gpu array for the reserve space of dropout forward
  // begin allocating gpu array for the states of dropout forward
  CUDA_CALL(cudaSetDevice(x6));
  float* x23 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x23, (size_t)x21));
  // end allocating gpu array for the states of dropout forward
  // begin creating dropout descriptor
  cudnnDropoutDescriptor_t x24;
  CUDNNCHECK(cudnnCreateDropoutDescriptor(&x24));
  CUDNNCHECK(cudnnSetDropoutDescriptor(x24, x7, 0.0, x23, x21, 1));
  // end creating dropout descriptor
  // begin allocating gpu array for the output of dropout
  CUDA_CALL(cudaSetDevice(x6));
  float* x25 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x25, (size_t)(18 * sizeof(float))));
  // end allocating gpu array for the output of dropout
  // begin dropout forward pass
  CUDNNCHECK(cudnnDropoutForward(x7, x24, x19, x9, x19, x25, x22, x20));
  // end dropout forward pass
  // begin computing ADD on GPU for size 9 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x171
  CUDA_CALL(cudaSetDevice(x6));
  float* x26 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x26, (size_t)(9 * sizeof(float))));
  x27<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x25, x26, 9);
  // end computing ADD on GPU for size 9 and type Float at device (pre-rename) x39 with left_operand x108 and right_operand x171
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x35 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x35, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x35, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x36 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x36, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, 0, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 9 and type Float at device (pre-name) x39 again binary file loss
  float* x37 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x37, x26, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/loss", x6, x37, 9);
  // end checking GPU array of size 9 and type Float at device (pre-name) x39 again binary file loss
  // begin initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x38 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x38, (size_t)(9 * sizeof(float))));
  x11<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x38, 1, 9);
  // end initializing fixed GPU array of size 9 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x243 and addition_operand x266
  CUDA_CALL(cudaSetDevice(x6));
  x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x36, x38, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x243 and addition_operand x266
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x230 and addition_operand x266
  CUDA_CALL(cudaSetDevice(x6));
  x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x35, x38, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x230 and addition_operand x266
  // begin allocating gpu array for the gradient of input of dropout
  CUDA_CALL(cudaSetDevice(x6));
  float* x46 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x46, (size_t)(18 * sizeof(float))));
  // end allocating gpu array for the gradient of input of dropout
  // begin finding dropout backward reserve bytes
  size_t x47 = (size_t)0;
  CUDNNCHECK(cudnnDropoutGetReserveSpaceSize(x19, &x47));
  // end finding dropout backward reserve bytes
  // begin finding dropout backward states bytes
  size_t x48 = (size_t)0;
  CUDNNCHECK(cudnnDropoutGetStatesSize(x7, &x48));
  // end finding dropout backward states bytes
  // begin allocating gpu array for the states of dropout backward
  CUDA_CALL(cudaSetDevice(x6));
  float* x49 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x49, (size_t)x48));
  // end allocating gpu array for the states of dropout backward
  // begin creating dropout descriptor
  cudnnDropoutDescriptor_t x50;
  CUDNNCHECK(cudnnCreateDropoutDescriptor(&x50));
  CUDNNCHECK(cudnnSetDropoutDescriptor(x50, x7, 0.0, x49, x48, 1));
  // end creating dropout descriptor
  // begin dropout backward pass
  CUDNNCHECK(cudnnDropoutBackward(x7, x50, x19, x36, x19, x46, x22, x47));
  // end dropout backward pass
  // begin computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x323
  CUDA_CALL(cudaSetDevice(x6));
  x39<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x10, x46, 9);
  // end computing ACCUM on GPU for size 9 and type Float at device (pre-rename) x39 with base_operand x67 and addition_operand x323
  // begin checking GPU array of size 9 and type Float at device (pre-name) x39 again binary file weight_grad
  float* x51 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x51, x10, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/weight_grad", x6, x51, 9);
  // end checking GPU array of size 9 and type Float at device (pre-name) x39 again binary file weight_grad
  // begin checking GPU array of size 9 and type Float at device (pre-name) x39 again binary file input_grad
  float* x52 = (float*)malloc(9 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x52, x35, (size_t)(9 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_rank("golden/input_grad", x6, x52, 9);
  // end checking GPU array of size 9 and type Float at device (pre-name) x39 again binary file input_grad
  cudnnDestroyTensorDescriptor(x19);
  CUDNNCHECK(cudnnDestroy(x7));
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
