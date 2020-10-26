/*****************************************
Emitting C Generated Code
*******************************************/
#include "nccl_header.h"
#include <string.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <mpi_header.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  ncclComm_t x1;
  int x2[1] = { 0 };
  NCCLCHECK(ncclCommInitAll(&x1, 1, x2));
  cudaStream_t x3;
  cudaStreamCreate(&x3);
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)0));
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)0));
  NCCLCHECK(ncclAllReduce(x4, x5, 0, ncclFloat, ncclSum, x1, x3));
  CUDA_CALL(cudaStreamSynchronize(x3));
  printf("end");
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
