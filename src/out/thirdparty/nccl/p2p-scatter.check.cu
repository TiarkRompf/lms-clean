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
  int x4 = MPI_Comm_size(MPI_COMM_WORLD, &x2);
  MPICHECK(x4);
  printf("myRank: %d, nRanks: %d\n", x1, x2);
  ncclUniqueId x5;
  ncclComm_t x6;
  cudaStream_t x7;
  if (x1 == 0) NCCLCHECK(ncclGetUniqueId(&x5));
  MPICHECK(MPI_Bcast(&x5, NCCL_UNIQUE_ID_BYTES, MPI_BYTE, 0, MPI_COMM_WORLD));
  float* x8 = (float*)malloc(1024 * sizeof(float));
  int x9 = 0;
  while (x9 != 1024) {
    x8[x9] = 2.0;
    x9 = x9 + 1;
  }
  CUDA_CALL(cudaSetDevice(x1));
  float** x10 = (float**)malloc(x2 * sizeof(float*));
  if (x1 == 0) {
    int x11 = x2;
    int x12 = 0;
    while (x12 != x11) {
      int x13 = x12;
      CUDA_CALL(cudaMalloc(&x10[x13], (size_t)(1024 * sizeof(float))));
      CUDA_CALL(cudaMemcpy(x10[x13], x8, (size_t)(1024 * sizeof(float)), cudaMemcpyHostToDevice));
      x12 = x12 + 1;
    }
  }
  float* x14 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x14, (size_t)(1024 * sizeof(float))));
  cudaError_t x15 = cudaStreamCreateWithFlags(&x7, cudaStreamDefault);
  CUDA_CALL(x15);
  ncclResult_t x16 = ncclCommInitRank(&x6, x2, x5, x1);
  NCCLCHECK(x16);
  NCCLCHECK(ncclGroupStart());
  if (x1 == 0) {
    int x17 = x2;
    int x18 = 0;
    while (x18 != x17) {
      int x19 = x18;
      NCCLCHECK(ncclSend(x10[x19], 1024, ncclFloat, x19, x6, x7));
      x18 = x18 + 1;
    }
  }
  NCCLCHECK(ncclRecv(x14, 1024, ncclFloat, 0, x6, x7));
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x7));
  float* x20 = (float*)malloc(1024 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x20, x14, (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x21 = 0;
  int x22 = 0;
  while (x22 != 1024) {
    if (x20[x22] != 2) x21 = x21 + 1;
    x22 = x22 + 1;
  }
  if (x1 == 0) {
    int x23 = x2;
    int x24 = 0;
    while (x24 != x23) {
      CUDA_CALL(cudaFree(x10[x24]));
      x24 = x24 + 1;
    }
  }
  CUDA_CALL(cudaFree(x14));
  NCCLCHECK(ncclCommDestroy(x6));
  MPICHECK(MPI_Finalize());
  if (x21 != 0) printf("[MPI Rank %d] Found %d errors.\n", x1, x21);
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
