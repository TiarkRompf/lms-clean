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
  CUDA_CALL(cudaSetDevice(x1));
  float* x4 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x4, (size_t)(33554432 * sizeof(float))));
  float* x5 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x5, (size_t)(33554432 * sizeof(float))));
  cudaStream_t x6;
  CUDA_CALL(cudaStreamCreateWithFlags(&x6, cudaStreamDefault));
  ncclComm_t x7;
  NCCLCHECK(ncclCommInitRank(&x7, x2, x3, x1));
  NCCLCHECK(ncclAllReduce(x4, x5, 33554432, ncclFloat, ncclSum, x7, x6));
  CUDA_CALL(cudaStreamSynchronize(x6));
  CUDA_CALL(cudaFree(x4));
  CUDA_CALL(cudaFree(x5));
  NCCLCHECK(ncclCommDestroy(x7));
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
