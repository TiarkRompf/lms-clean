/*****************************************
Emitting C Generated Code
*******************************************/
#include <nccl_header.h>
#include <string.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <mpi_header.h>
#include <nccl.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  ncclComm_t x1;
  int x2[1] = { 0 };
  cudaStream_t x3;
  CUDA_CALL(cudaSetDevice(0));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(33554432 * sizeof(int))));
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(33554432 * sizeof(int))));
  CUDA_CALL(cudaMemset(x4, 1, 33554432));
  CUDA_CALL(cudaMemset(x5, 0, 33554432));
  CUDA_CALL(cudaStreamCreateWithFlags(&x3, cudaStreamDefault));
  NCCLCHECK(ncclCommInitAll(&x1, 1, x2));
  NCCLCHECK(ncclAllReduce(x4, x5, 33554432, ncclFloat, ncclSum, x1, x3));
  CUDA_CALL(cudaSetDevice(0));
  CUDA_CALL(cudaStreamSynchronize(x3));
  CUDA_CALL(cudaFree(x4));
  CUDA_CALL(cudaFree(x5));
  NCCLCHECK(ncclCommDestroy(x1));
  printf("Success \n");
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
