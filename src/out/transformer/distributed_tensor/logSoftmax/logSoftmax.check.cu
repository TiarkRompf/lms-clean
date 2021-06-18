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
  // This is cuda softmax (for larger; >=1024 inputs). Performs softmax on last dim.
  // arg0: input: <outerSize x lastDimSize>
  // arg1: output: <outerSize x lastDimSize>
  // arg2: lastDimSize: size of the last dimension (i.e., the softmax dim)
  // invocation assumption: <<<dim3(outerSize,1,1), dim3(1024,1,1), 1024*4>>>
  extern __shared__ float x23[];
  float* x24 = x20 + x22 * blockIdx.x;
  float* x25 = x21 + x22 * blockIdx.x;
  float x26 = -INFINITY;
  int x27 = blockDim.x;
  // thread local reduce
  float x28 = x26;
  int x29 = threadIdx.x;
  while (x29 < x22) {
    float x30 = x28;
    float x31 = x24[x29];
    x28 = x30 < x31 ? x31 : x30;
    x29 = x29 + x27;
  }
  x23[threadIdx.x] = x28;
  __syncthreads();
  // reduce to the first warp
  float x32 = x26;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x33 = threadIdx.x;
    int x34 = NVIDIA_WARP_SIZE;
    int x35 = 0;
    while (x35 != x34) {
      float x36 = x32;
      float x37 = x23[x33 * NVIDIA_WARP_SIZE + x35];
      x32 = x36 < x37 ? x37 : x36;
      x35 = x35 + 1;
    }
    x23[x33] = x32;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x38 = x26;
    int x39 = blockDim.x / NVIDIA_WARP_SIZE;
    int x40 = 0;
    while (x40 != x39) {
      float x41 = x38;
      float x42 = x23[x40];
      x38 = x41 < x42 ? x42 : x41;
      x40 = x40 + 1;
    }
    x23[0] = x38;
  }
  __syncthreads();
  float x43 = 0.0;
  int x44 = blockDim.x;
  int x45 = threadIdx.x;
  while (x45 < x22) {
    int x46 = x45;
    float x47 = x24[x46] - x23[0];
    x43 = x43 + expf(x47);
    x25[x46] = x47;
    x45 = x45 + x44;
  }
  x23[threadIdx.x] = x43;
  __syncthreads();
  // reduce to the first warp
  float x48 = 0.0;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x49 = threadIdx.x;
    int x50 = NVIDIA_WARP_SIZE;
    int x51 = 0;
    while (x51 != x50) {
      x48 = x48 + x23[x49 * NVIDIA_WARP_SIZE + x51];
      x51 = x51 + 1;
    }
    x23[x49] = x48;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x52 = 0.0;
    int x53 = blockDim.x / NVIDIA_WARP_SIZE;
    int x54 = 0;
    while (x54 != x53) {
      x52 = x52 + x23[x54];
      x54 = x54 + 1;
    }
    x23[0] = x52;
  }
  __syncthreads();
  int x55 = blockDim.x;
  int x56 = threadIdx.x;
  while (x56 < x22) {
    int x57 = x56;
    x25[x57] = x25[x57] - log(x23[0]);
    x56 = x56 + x55;
  }
}
__global__ void x59(float* x60, float* x61, float* x62, int x63) {
  // begin generating kernel function for ADD of type Float
  int x64 = gridDim.x * blockDim.x;
  int x65 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x65 < x63) {
    int x66 = x65;
    x62[x66] = x60[x66] + x61[x66];
    x65 = x65 + x64;
  }
  // end generating kernel function for ADD of type Float
}
__global__ void x71(float* x72, float* x73, int x74) {
  // begin generating kernel function for ACCUM of type Float
  int x75 = gridDim.x * blockDim.x;
  int x76 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x76 < x74) {
    int x77 = x76;
    x72[x77] = x72[x77] + x73[x77];
    x76 = x76 + x75;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x79(float* x80, float* x81, float* x82, int x83) {
  // This is cuda softmax (for larger; >=1024 inputs). Performs softmax on last dim.
  // arg0: gradInput: the gradient of the original input (i.e., the softmax input) - This is an output of the kernel
  // arg1: gradOutput: gradient of softmax output, coming from upstream; An Input to the kernel
  // arg2: output: output of softmax forward pass
  // arg3: size: last dimension size
  // invocation assumption: <<<dim3(outerSize,1,1), dim3(1024,1,1), 1024*4>>>
  extern __shared__ float x84[];
  float* x85 = x80 + x83 * blockIdx.x;
  float* x86 = x81 + x83 * blockIdx.x;
  float* x87 = x82 + x83 * blockIdx.x;
  int x88 = threadIdx.x;
  int x89 = blockDim.x;
  float x90 = 0.0;
  int x91 = x88;
  while (x91 < x83) {
    x90 = x90 + x86[x91];
    x91 = x91 + x89;
  }
  x84[threadIdx.x] = x90;
  __syncthreads();
  // reduce to the first warp
  float x92 = 0.0;
  if (threadIdx.x < blockDim.x / NVIDIA_WARP_SIZE) {
    int x93 = threadIdx.x;
    int x94 = NVIDIA_WARP_SIZE;
    int x95 = 0;
    while (x95 != x94) {
      x92 = x92 + x84[x93 * NVIDIA_WARP_SIZE + x95];
      x95 = x95 + 1;
    }
    x84[x93] = x92;
  }
  __syncthreads();
  // reduce to the first thread
  if (threadIdx.x == 0) {
    float x96 = 0.0;
    int x97 = blockDim.x / NVIDIA_WARP_SIZE;
    int x98 = 0;
    while (x98 != x97) {
      x96 = x96 + x84[x98];
      x98 = x98 + 1;
    }
    x84[0] = x96;
  }
  __syncthreads();
  int x99 = x88;
  while (x99 < x83) {
    int x100 = x99;
    x85[x100] = x86[x100] - x84[0] * expf(x87[x100]);
    x99 = x99 + x89;
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
  // begin initializing GPU array of size 17056 and type Float
  float* x7 = (float*)malloc(17056 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(17056 * sizeof(float))));
  scan_float_array(x7, 17056, "golden/weight_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x8, x7, (size_t)(17056 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 17056 and type Float
  // begin initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x9 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x9, (size_t)(17056 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, 0, 17056);
  // end initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  // begin initializing GPU array of size 17056 and type Float
  float* x16 = (float*)malloc(17056 * sizeof(float));
  CUDA_CALL(cudaSetDevice(x6));
  float* x17 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x17, (size_t)(17056 * sizeof(float))));
  scan_float_array(x16, 17056, "golden/input_rank_%d.data", x6);
  CUDA_CALL(cudaMemcpy(x17, x16, (size_t)(17056 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing GPU array of size 17056 and type Float
  // begin allocating output array of size 17056 and type Float for tensor_logsoftmax
  CUDA_CALL(cudaSetDevice(x6));
  float* x18 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x18, (size_t)(17056 * sizeof(float))));
  // end allocating output array of size 17056 and type Float for tensor_logsoftmax
  // begin calling softmax kernel
  x19<<<dim3(32, 1, 1), dim3(1024, 1, 1), 4096>>>(x8, x18, 533);
  // begin calling softmax kernel
  // begin computing ADD on GPU for size 17056 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x120
  CUDA_CALL(cudaSetDevice(x6));
  float* x58 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x58, (size_t)(17056 * sizeof(float))));
  x59<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x17, x18, x58, 17056);
  // end computing ADD on GPU for size 17056 and type Float at device (pre-rename) x39 with left_operand x103 and right_operand x120
  // begin initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x67 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x67, (size_t)(17056 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x67, 0, 17056);
  // end initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  // begin initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x68 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x68, (size_t)(17056 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x68, 0, 17056);
  // end initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  // begin checking GPU array of size 17056 and type Float
  float* x69 = (float*)malloc(17056 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x69, x58, (size_t)(17056 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x69, 17056, "golden/loss_rank_%d.data", x6);
  // end checking GPU array of size 17056 and type Float
  // begin initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  CUDA_CALL(cudaSetDevice(x6));
  float* x70 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x70, (size_t)(17056 * sizeof(float))));
  x10<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x70, 1, 17056);
  // end initializing fixed GPU array of size 17056 and type Float and device (pre-rename) x39
  // begin computing ACCUM on GPU for size 17056 and type Float at device (pre-rename) x39 with base_operand x417 and addition_operand x440
  CUDA_CALL(cudaSetDevice(x6));
  x71<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x68, x70, 17056);
  // end computing ACCUM on GPU for size 17056 and type Float at device (pre-rename) x39 with base_operand x417 and addition_operand x440
  // begin computing ACCUM on GPU for size 17056 and type Float at device (pre-rename) x39 with base_operand x404 and addition_operand x440
  CUDA_CALL(cudaSetDevice(x6));
  x71<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x67, x70, 17056);
  // end computing ACCUM on GPU for size 17056 and type Float at device (pre-rename) x39 with base_operand x404 and addition_operand x440
  // begin allocating gradient input array of size 17056 and type Float for tensor_logsoftmax
  CUDA_CALL(cudaSetDevice(x6));
  float* x78 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x78, (size_t)(17056 * sizeof(float))));
  // end allocating gradient input array of size 17056 and type Float for tensor_logsoftmax
  // begin calling softmax gradient kernel
  x79<<<dim3(32, 1, 1), dim3(1024, 1, 1), 4096>>>(x78, x68, x18, 533);
  // end calling softmax gradient kernel
  // begin computing ACCUM on GPU for size 17056 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x497
  CUDA_CALL(cudaSetDevice(x6));
  x71<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x9, x78, 17056);
  // end computing ACCUM on GPU for size 17056 and type Float at device (pre-rename) x39 with base_operand x62 and addition_operand x497
  // begin checking GPU array of size 17056 and type Float
  float* x101 = (float*)malloc(17056 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x101, x9, (size_t)(17056 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x101, 17056, "golden/weight_grad_rank_%d.data", x6);
  // end checking GPU array of size 17056 and type Float
  // begin checking GPU array of size 17056 and type Float
  float* x102 = (float*)malloc(17056 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x102, x67, (size_t)(17056 * sizeof(float)), cudaMemcpyDeviceToHost));
  check_float_array_with_file(x102, 17056, "golden/input_grad_rank_%d.data", x6);
  // end checking GPU array of size 17056 and type Float
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
