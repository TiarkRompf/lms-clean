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
  ncclDataType_t x17 = ncclFloat;
  NCCLCHECK(ncclGroupStart());
  if (x1 == 0) {
    int x18 = x2;
    int x19 = 0;
    while (x19 != x18) {
      int x20 = x19;
      NCCLCHECK(ncclSend(x10[x20], 1024, x17, x20, x6, x7));
      x19 = x19 + 1;
    }
  }
  NCCLCHECK(ncclRecv(x14, 1024, x17, 0, x6, x7));
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x7));
  float* x21 = (float*)malloc(1024 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x21, x14, (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x22 = 0;
  int x23 = 0;
  while (x23 != 1024) {
    if (x21[x23] != 2) x22 = x22 + 1;
    x23 = x23 + 1;
  }
  if (x1 == 0) {
    int x24 = x2;
    int x25 = 0;
    while (x25 != x24) {
      CUDA_CALL(cudaFree(x10[x25]));
      x25 = x25 + 1;
    }
  }
  CUDA_CALL(cudaFree(x14));
  NCCLCHECK(ncclCommDestroy(x6));
  MPICHECK(MPI_Finalize());
  if (x22 != 0) printf("[MPI Rank %d] Found %d errors.\n", x1, x22);
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
