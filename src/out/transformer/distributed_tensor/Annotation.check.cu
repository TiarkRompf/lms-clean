/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x154(float* x131, float x132, int x133) {
  int x136 = gridDim.x * blockDim.x;
  int x143 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x143 < x133) {
    x131[x143] = x132;
    x143 = x143 + x136;
  }
}
__global__ void x261(float* x234, float* x235, float* x236, int x237) {
  int x240 = gridDim.x * blockDim.x;
  int x247 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x247 < x237) {
    int x252 = x247;
    x236[x252] = x234[x252] * x235[x252];
    x247 = x247 + x240;
  }
}
__global__ void x298(float* x272, float* x273, int x274) {
  int x277 = gridDim.x * blockDim.x;
  int x284 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x284 < x274) {
    int x289 = x284;
    x272[x289] = x272[x289] + x273[x289];
    x284 = x284 + x277;
  }
}
__global__ void x341(float* x309, float* x310, float* x311, int x312) {
  int x315 = gridDim.x * blockDim.x;
  int x322 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x322 < x312) {
    int x327 = x322;
    float x332 = x311[x327] * 0.5 + x310[x327];
    x309[x327] = x309[x327] - x332 * 1.0E-4;
    x311[x327] = x332;
    x322 = x322 + x315;
  }
}
/**************** Snippet ****************/
void Snippet(int x47) {
  // begin setting up the MPI/NCCL environment
  int x49 = 0;
  int x50 = 0;
  MPICHECK(MPI_Init(NULL, NULL));
  MPICHECK(MPI_Comm_rank(MPI_COMM_WORLD, &x50));
  int x59 = MPI_Comm_size(MPI_COMM_WORLD, &x49);
  MPICHECK(x59);
  MPICHECK(MPI_Barrier(MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(x50));
  ncclUniqueId x67;
  NCCLCHECK(ncclGetUniqueId(&x67));
  MPICHECK(MPI_Bcast(&x67, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, MPI_COMM_WORLD));
  ncclComm_t x75;
  NCCLCHECK(ncclCommInitRank(&x75, x49, x67, x50));
  cudaStream_t x80;
  CUDA_CALL(cudaStreamCreateWithFlags(&x80, cudaStreamNonBlocking));
  int x85 = x50;
  // end setting up the MPI/NCCL environment
  // begin initializing random GPU array of size 512 and type Float at device (pre-rename) x85
  float* x88 = (float*)malloc(512 * sizeof(float));
  int x90 = 0;
  bool x92 = x90 != 512;
  int x103 = x90 + 1;
  while (x92) {
    x88[x90] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
    x90 = x103;
  }
  CUDA_CALL(cudaSetDevice(x85));
  float* x108 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x108, (size_t)(512 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x108, x88, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
  // end initializing random GPU array of size 512 and type Float at device (pre-rename) x85
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x85
  CUDA_CALL(cudaSetDevice(x85));
  float* x124 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x124, (size_t)(512 * sizeof(float))));
  x154<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x124, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x85
  // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x85
  CUDA_CALL(cudaSetDevice(x85));
  float* x162 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x162, (size_t)(512 * sizeof(float))));
  x154<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x162, 0, 512);
  // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x85
  int x172 = 0;
  while (x172 != 5) {
    // begin initializing random GPU array of size 512 and type Float at device (pre-rename) x85
    float* x178 = (float*)malloc(512 * sizeof(float));
    int x179 = 0;
    bool x181 = x179 != 512;
    int x192 = x179 + 1;
    while (x181) {
      x178[x179] = (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX;
      x179 = x192;
    }
    CUDA_CALL(cudaSetDevice(x85));
    float* x197 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x197, (size_t)(512 * sizeof(float))));
    CUDA_CALL(cudaMemcpy(x197, x178, (size_t)(512 * sizeof(float)), cudaMemcpyHostToDevice));
    // end initializing random GPU array of size 512 and type Float at device (pre-rename) x85
    // begin initializing fixed GPU array of size 512 and type Float and device (pre-rename) x85
    CUDA_CALL(cudaSetDevice(x85));
    float* x213 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x213, (size_t)(512 * sizeof(float))));
    x154<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x213, 1, 512);
    // end initializing fixed GPU array of size 512 and type Float and device (pre-rename) x85
    // begin computing MULT on GPU for size 512 and type Float at device (pre-rename) x85 with left_operand x197 and right_operand x213
    CUDA_CALL(cudaSetDevice(x85));
    float* x226 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x226, (size_t)(512 * sizeof(float))));
    // begin generating kernel function for MULT of type Float
    // end generating kernel function for MULT of type Float
    x261<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x197, x213, x226, 512);
    // end computing MULT on GPU for size 512 and type Float at device (pre-rename) x85 with left_operand x197 and right_operand x213
    // begin computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x85 with base_operand x124 and addition_operand x226
    CUDA_CALL(cudaSetDevice(x85));
    // begin generating kernel function for ACCUM of type Float
    // end generating kernel function for ACCUM of type Float
    x298<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x124, x226, 512);
    // end computing ACCUM on GPU for size 512 and type Float at device (pre-rename) x85 with base_operand x124 and addition_operand x226
    // begin computing SGD on GPU for size 512 and type Float at device (pre-name) x85 with weight x108, grad x124, and momentum x162
    CUDA_CALL(cudaSetDevice(x85));
    // begin generating kernel function for SGD of type Float
    // end generating kernel function for SGD of type Float
    x341<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x108, x124, x162, 512);
    // end computing SGD on GPU for size 512 and type Float at device (pre-name) x85 with weight x108, grad x124, and momentum x162
    x172 = x172 + 1;
  }
  // Only declare recv buffer if this is the root
  bool x352 = x85 == 0;
  float* x366 = x352 ? ({
    float* x354 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x354, (size_t)(1024 * sizeof(float))));
    x354;
  }) : ({
    float* x361 = (float*)malloc(0 * sizeof(float));
    CUDA_CALL(cudaMalloc(&x361, (size_t)0));
    x361;
  });
  // Gather by groups of NCCL send/recv
  NCCLCHECK(ncclGroupStart());
  NCCLCHECK(ncclSend(x20, (size_t)1024, ncclFloat32, 0, x75, x80));
  if (x352) {
    int x376 = 0;
    bool x378 = x376 != x49;
    int x381 = x376 + 1;
    float* x383 = x366 + x376 * 512;
    while (x378) {
      NCCLCHECK(ncclRecv(x383, (size_t)1024, ncclFloat32, x376, x75, x80));
      x376 = x381;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  // print the array only if this is the root
  if (x352) {
    // begin copying GPU array x366 to CPU and print for size 1024 and type Float
    float* x397 = (float*)malloc(1024 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x397, x366, (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x405 = 0;
    bool x407 = x405 != 1024;
    int x411 = x405 + 1;
    while (x407) {
      printf("%f ", x397[x405]);
      x405 = x411;
    }
    printf("\n");
    // end copying GPU array x366 to CPU and print for size 1024 and type Float
  }
  printf("compile");
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
