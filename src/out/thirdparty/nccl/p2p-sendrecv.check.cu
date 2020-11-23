/*****************************************
Emitting C Generated Code
*******************************************/
#include "nccl_header.h"
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "mpi_header.h"
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1 = 0;
  int x2 = 0;
  MPICHECK(MPI_Init(NULL, NULL));
  int x3 = MPI_Comm_rank(MPI_COMM_WORLD, &x1);
  MPICHECK(x3);
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x2));
  printf("myRank: %d, nRanks: %d\n", x1, x2);
  ncclUniqueId x4;
  ncclComm_t x5;
  cudaStream_t x6;
  if (x1 == 0) NCCLCHECK(ncclGetUniqueId(&x4));
  MPICHECK(MPI_Bcast(&x4, NCCL_UNIQUE_ID_BYTES, MPI_BYTE, 0, MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(x1));
  float* x7 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x7, (size_t)(1024 * sizeof(float))));
  float* x8 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(1024 * sizeof(float))));
  float* x9 = (float*)malloc(1024 * sizeof(float));
  int x10 = 0;
  while (x10 != 1024) {
    x9[x10] = 2.0;
    x10 = x10 + 1;
  }
  CUDA_CALL(cudaMemcpy(x7, x9, 1024, cudaMemcpyHostToDevice));
  CUDA_CALL(cudaStreamCreateWithFlags(&x6, cudaStreamDefault));
  NCCLCHECK(ncclCommInitRank(&x5, x2, x4, x1));
  int x11 = x1 == 0 ? 1 : 0;
  NCCLCHECK(ncclGroupStart());
  NCCLCHECK(ncclSend(x7, 1024, ncclFloat, x11, x5, x6));
  NCCLCHECK(ncclRecv(x8, 1024, ncclFloat, x11, x5, x6));
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x6));
  CUDA_CALL(cudaMemcpy(x9, x8, 1024, cudaMemcpyDeviceToHost));
  int x12 = 0;
  int x13 = 0;
  while (x13 != 1024) {
    if (x9[0] != 2) x12 = x12 + 1;
    x13 = x13 + 1;
  }
  CUDA_CALL(cudaFree(x7));
  CUDA_CALL(cudaFree(x8));
  NCCLCHECK(ncclCommDestroy(x5));
  MPICHECK(MPI_Finalize());
  if (x12 != 0) printf("[MPI Rank %d] Found %d errors.\n", x1, x12);
  else printf("[MPI Rank %d] Success \n", x1);
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
