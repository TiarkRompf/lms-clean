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
  float* x7 = (float*)malloc(1024 * sizeof(float));
  int x8 = 0;
  while (x8 != 1024) {
    x7[x8] = 2.0;
    x8 = x8 + 1;
  }
  CUDA_CALL(cudaSetDevice(x1));
  float** x9 = (float**)malloc(x2 * sizeof(float*));
  float** x10 = (float**)malloc(x2 * sizeof(float*));
  int x11 = x2;
  int x12 = 0;
  while (x12 != x11) {
    int x13 = x12;
    CUDA_CALL(cudaMalloc(&x9[x13], (size_t)(1024 * sizeof(float))));
    CUDA_CALL(cudaMalloc(&x10[x13], (size_t)(1024 * sizeof(float))));
    CUDA_CALL(cudaMemcpy(x9[x13], x7, (size_t)(1024 * sizeof(float)), cudaMemcpyHostToDevice));
    x12 = x12 + 1;
  }
  CUDA_CALL(cudaStreamCreateWithFlags(&x6, cudaStreamDefault));
  NCCLCHECK(ncclCommInitRank(&x5, x2, x4, x1));
  int x14 = x2;
  ncclDataType_t x15 = ncclFloat;
  NCCLCHECK(ncclGroupStart());
  int x16 = 0;
  while (x16 != x14) {
    int x17 = x16;
    NCCLCHECK(ncclSend(x9[x17], 1024, x15, x17, x5, x6));
    NCCLCHECK(ncclRecv(x10[x17], 1024, x15, x17, x5, x6));
    x16 = x16 + 1;
  }
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x6));
  int x18 = 0;
  int x19 = x2;
  int x20 = 0;
  while (x20 != x19) {
    float* x21 = (float*)malloc(1024 * sizeof(float));
    CUDA_CALL(cudaMemcpy(x21, x10[x20], (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
    int x22 = 0;
    while (x22 != 1024) {
      if (x21[x22] != 2) x18 = x18 + 1;
      x22 = x22 + 1;
    }
    x20 = x20 + 1;
  }
  int x23 = x2;
  int x24 = 0;
  while (x24 != x23) {
    int x25 = x24;
    CUDA_CALL(cudaFree(x9[x25]));
    CUDA_CALL(cudaFree(x10[x25]));
    x24 = x24 + 1;
  }
  NCCLCHECK(ncclCommDestroy(x5));
  MPICHECK(MPI_Finalize());
  if (x18 != 0) printf("[MPI Rank %d] Found %d errors.\n", x1, x18);
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
