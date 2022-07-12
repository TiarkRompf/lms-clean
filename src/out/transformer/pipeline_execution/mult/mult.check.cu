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
__global__ void x38(float* x39, float* x40, int x41) {
  // begin generating kernel function for ACCUM of type Float
  int x42 = gridDim.x * blockDim.x;
  int x43 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x43 < x41) {
    int x44 = x43;
    x39[x44] = x39[x44] + x40[x44];
    x43 = x43 + x42;
  }
  // end generating kernel function for ACCUM of type Float
}
__global__ void x45(float* x46, float* x47, float* x48, int x49) {
  // begin generating kernel function for SGD of type Float
  int x50 = gridDim.x * blockDim.x;
  int x51 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x51 < x49) {
    int x52 = x51;
    float x53 = x48[x52] * 0.5 + x47[x52];
    x46[x52] = x46[x52] - x53 * 1.0E-4;
    x48[x52] = x53;
    x51 = x51 + x50;
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
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x1));
  MPICHECK(MPI_Barrier(MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(x2));
  ncclUniqueId x3;
  NCCLCHECK(ncclGetUniqueId(&x3));
  MPICHECK(MPI_Bcast(&x3, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, MPI_COMM_WORLD));
  ncclComm_t x4;
  ncclResult_t x5 = ncclCommInitRank(&x4, x1, x3, x2);
  NCCLCHECK(x5);
  cudaStream_t x6;
  cudaError_t x7 = cudaStreamCreateWithFlags(&x6, cudaStreamNonBlocking);
  CUDA_CALL(x7);
  // begin setting up the local MPI/NCCL environment
  MPI_Comm x8;
  int x9 = x2;
  MPICHECK(MPI_Comm_split(MPI_COMM_WORLD, x9 / 2, x9, &x8));
  int x10 = 0;
  int x11 = 0;
  int x12 = MPI_Comm_rank(x8, &x11);
  MPICHECK(x12);
  MPICHECK(MPI_Comm_size(x8, &x10));
  ncclUniqueId x13;
  NCCLCHECK(ncclGetUniqueId(&x13));
  MPICHECK(MPI_Bcast(&x13, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, x8));
  ncclComm_t x14;
  NCCLCHECK(ncclCommInitRank(&x14, x10, x13, x11));
  int x15 = x2;
  // end setting up the local MPI/NCCL environment
  // end setting up the MPI/NCCL environment
  if (x15 >= 0 && x15 < 2) {
    int x16 = x11;
    // begin initializing GPU array of size 512 and type Float
    float* x17 = (float*)malloc(512 * sizeof(float));
    CUDA_CALL(cudaSetDevice(x15));
    float* x18 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x18, (size_t)(512 * sizeof(float))));
    scan_float_array(x17, 512, "golden/weight_rank_%d.data", x16);
    CUDA_CALL(cudaMemcpy(x18, x17, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
    // end initializing GPU array of size 512 and type Float
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
    CUDA_CALL(cudaSetDevice(x15));
    float* x19 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x19, (size_t)(512 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, 0, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
    CUDA_CALL(cudaSetDevice(x15));
    float* x26 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x26, (size_t)(512 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x26, 0, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
    // begin initializing fixed GPU array of size 2048 and type Float and device (pre-rename) x66
    CUDA_CALL(cudaSetDevice(x15));
    float* x27 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x27, (size_t)(2048 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x27, 0, 2048);
    // end initializing fixed GPU array of size 2048 and type Float and device (pre-rename) x66
    int x28 = 0;
    int x37 = x15 + 2;
    while (x28 != 3) {
      int x54 = 0;
      while (x54 != 4) {
        // begin initializing GPU array of size 512 and type Float
        float* x55 = (float*)malloc(512 * sizeof(float));
        CUDA_CALL(cudaSetDevice(x15));
        float* x56 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x56, (size_t)(512 * sizeof(float))));
        scan_float_array(x55, 512, "golden/input1_rank_%d.data", x16);
        CUDA_CALL(cudaMemcpy(x56, x55, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
        // end initializing GPU array of size 512 and type Float
        CUDA_CALL(cudaMemcpy(x27 + 512 * x54, x56, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToDevice));
        // begin computing MULT on GPU for size 512 and type Float at device (pre-rename) x66 with left_operand x172 and right_operand x77
        CUDA_CALL(cudaSetDevice(x15));
        float* x57 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x57, (size_t)(512 * sizeof(float))));
        x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x56, x18, x57, 512);
        // end computing MULT on GPU for size 512 and type Float at device (pre-rename) x66 with left_operand x172 and right_operand x77
        CUDA_CALL(cudaStreamSynchronize(0));
        NCCLCHECK(ncclSend(x57, (size_t)512, ncclFloat32, x37, x4, x6));
        x54 = x54 + 1;
      }
      int x58 = 0;
      while (x58 != 4) {
        // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        CUDA_CALL(cudaSetDevice(x15));
        float* x59 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x59, (size_t)(512 * sizeof(float))));
        x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x59, 0, 512);
        // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        CUDA_CALL(cudaStreamSynchronize(0));
        NCCLCHECK(ncclRecv(x59, (size_t)512, ncclFloat32, x37, x4, x6));
        CUDA_CALL(cudaStreamSynchronize(x6));
        // begin computing MULT on GPU for size 512 and type Float at device (pre-rename) x66 with left_operand x256 and right_operand x260
        CUDA_CALL(cudaSetDevice(x15));
        float* x60 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x60, (size_t)(512 * sizeof(float))));
        x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x27 + 512 * x58, x59, x60, 512);
        // end computing MULT on GPU for size 512 and type Float at device (pre-rename) x66 with left_operand x256 and right_operand x260
        // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x66 with base_operand x94 and addition_operand x282
        CUDA_CALL(cudaSetDevice(x15));
        x38<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x19, x60, 512);
        // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x66 with base_operand x94 and addition_operand x282
        x58 = x58 + 1;
      }
      // begin computing SGD on GPU for size 512 and type Float at device (pre-name) x66 with weight x77, grad x94, and momentum x134
      CUDA_CALL(cudaSetDevice(x15));
      x45<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x18, x19, x26, 512);
      // end computing SGD on GPU for size 512 and type Float at device (pre-name) x66 with weight x77, grad x94, and momentum x134
      // begin checking GPU array of size 512 and type Float
      float* x61 = (float*)malloc(512 * sizeof(float));
      CUDA_CALL(cudaMemcpy(x61, x19, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
      check_float_array_with_file(x61, 512, "golden/weight_grad_rank_%d.data", x16);
      // end checking GPU array of size 512 and type Float
      // begin checking GPU array of size 512 and type Float
      float* x62 = (float*)malloc(512 * sizeof(float));
      CUDA_CALL(cudaMemcpy(x62, x18, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToHost));
      check_float_array_with_file(x62, 512, "golden/weight_rank_%d.data", x16);
      // end checking GPU array of size 512 and type Float
      x28 = x28 + 1;
    }
  }
  if (x15 >= 2 && x15 < 4) {
    int x16 = x11;
    // begin initializing fixed GPU array of size 2048 and type Float and device (pre-rename) x66
    CUDA_CALL(cudaSetDevice(x15));
    float* x63 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x63, (size_t)(2048 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x63, 0, 2048);
    // end initializing fixed GPU array of size 2048 and type Float and device (pre-rename) x66
    // begin initializing fixed GPU array of size 2048 and type Float and device (pre-rename) x66
    CUDA_CALL(cudaSetDevice(x15));
    float* x64 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x64, (size_t)(2048 * sizeof(float))));
    x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x64, 0, 2048);
    // end initializing fixed GPU array of size 2048 and type Float and device (pre-rename) x66
    int x65 = 0;
    int x66 = x15 - 2;
    while (x65 != 3) {
      int x67 = 0;
      while (x67 != 4) {
        // begin initializing GPU array of size 512 and type Float
        float* x68 = (float*)malloc(512 * sizeof(float));
        CUDA_CALL(cudaSetDevice(x15));
        float* x69 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x69, (size_t)(512 * sizeof(float))));
        scan_float_array(x68, 512, "golden/input2_rank_%d.data", x16);
        CUDA_CALL(cudaMemcpy(x69, x68, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
        // end initializing GPU array of size 512 and type Float
        int x70 = 512 * x67;
        CUDA_CALL(cudaMemcpy(x64 + x70, x69, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToDevice));
        // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        CUDA_CALL(cudaSetDevice(x15));
        float* x71 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x71, (size_t)(512 * sizeof(float))));
        x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x71, 0, 512);
        // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        CUDA_CALL(cudaStreamSynchronize(0));
        NCCLCHECK(ncclRecv(x71, (size_t)512, ncclFloat32, x66, x4, x6));
        CUDA_CALL(cudaStreamSynchronize(x6));
        CUDA_CALL(cudaMemcpy(x63 + x70, x71, (size_t)(512 * sizeof(float)), cudaMemcpyDeviceToDevice));
        x67 = x67 + 1;
      }
      int x72 = 0;
      while (x72 != 4) {
        // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        CUDA_CALL(cudaSetDevice(x15));
        float* x73 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x73, (size_t)(512 * sizeof(float))));
        x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x73, 0, 512);
        // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        CUDA_CALL(cudaSetDevice(x15));
        float* x74 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x74, (size_t)(512 * sizeof(float))));
        x20<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x74, 1, 512);
        // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x66
        // begin computing MULT on GPU for size 512 and type Float at device (pre-rename) x66 with left_operand x523 and right_operand x527
        CUDA_CALL(cudaSetDevice(x15));
        float* x75 = (float*)malloc(0 * sizeof(float));
        CUDA_CALL(cudaMalloc(&x75, (size_t)(512 * sizeof(float))));
        x29<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x64 + 512 * x72, x74, x75, 512);
        // end computing MULT on GPU for size 512 and type Float at device (pre-rename) x66 with left_operand x523 and right_operand x527
        // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x66 with base_operand x512 and addition_operand x540
        CUDA_CALL(cudaSetDevice(x15));
        x38<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x73, x75, 512);
        // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x66 with base_operand x512 and addition_operand x540
        CUDA_CALL(cudaStreamSynchronize(0));
        NCCLCHECK(ncclSend(x73, (size_t)512, ncclFloat32, x66, x4, x6));
        x72 = x72 + 1;
      }
      x65 = x65 + 1;
    }
  }
  NCCLCHECK(ncclCommDestroy(x14));
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
