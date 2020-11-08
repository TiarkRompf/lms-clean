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
  int x1 = 0;
  int x2 = 0;
  MPICHECK(MPI_Init(NULL, NULL));
  MPICHECK(MPI_Comm_rank(MPI_COMM_WORLD, &x1));
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x2));
  ncclUniqueId x3;
  if (x1 == 0) NCCLCHECK(ncclGetUniqueId(&x3));
  MPICHECK(MPI_Bcast(&x3, NCCL_UNIQUE_ID_BYTES, MPI_BYTE, 0, MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(0));
  cudaStream_t x4;
  CUDA_CALL(cudaStreamCreateWithFlags(&x4, cudaStreamDefault));
  ncclComm_t x5;
  NCCLCHECK(ncclCommInitRank(&x5, x2, x3, x1));
  float* x6 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x6, (size_t)(33554432 * sizeof(float))));
  float* x7 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x7, (size_t)(33554432 * sizeof(float))));
  float* x8 = (float*)malloc(33554432 * sizeof(float));
  int x9 = 0;
  while (x9 != 33554432) {
    x8[x9] = 0.0;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaMemcpy(x6, x8, 33554432, cudaMemcpyHostToDevice));
  float* x10 = (float*)malloc(33554432 * sizeof(float));
  int x11 = 0;
  while (x11 != 33554432) {
    x10[x11] = 1.0;
    x11 = x11 + 1;
  }
  CUDA_CALL(cudaMemcpy(x7, x10, 33554432, cudaMemcpyHostToDevice));
  NCCLCHECK(ncclGroupStart());
  NCCLCHECK(ncclSend(x6, 33554432, ncclFloat, 1, x5, x4));
  NCCLCHECK(ncclRecv(x7, 33554432, ncclFloat, 0, x5, x4));
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x4));
  CUDA_CALL(cudaMemcpy(x8, x6, 33554432, cudaMemcpyDeviceToHost));
  int x12 = 0;
  while (x12 != 33554432) {
    if (x8[0] != 1) printf("error");
    x12 = x12 + 1;
  }
  CUDA_CALL(cudaMemcpy(x10, x7, 33554432, cudaMemcpyDeviceToHost));
  int x13 = 0;
  while (x13 != 33554432) {
    if (x10[0] != 0) printf("error");
    x13 = x13 + 1;
  }
  CUDA_CALL(cudaFree(x6));
  CUDA_CALL(cudaFree(x7));
  NCCLCHECK(ncclCommDestroy(x5));
  MPICHECK(MPI_Finalize());
  printf("[MPI Rank %d] Success \n", x1);
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
