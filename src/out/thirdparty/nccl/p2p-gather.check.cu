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
  float* x10 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x10, (size_t)(1024 * sizeof(float))));
  CUDA_CALL(cudaMemcpy(x10, x8, (size_t)(1024 * sizeof(float)), cudaMemcpyHostToDevice));
  float** x11 = (float**)malloc(x2 * sizeof(float*));
  if (x1 == 0) {
    int x12 = x2;
    int x13 = 0;
    while (x13 != x12) {
      CUDA_CALL(cudaMalloc(&x11[x13], (size_t)(1024 * sizeof(float))));
      x13 = x13 + 1;
    }
  }
  cudaError_t x14 = cudaStreamCreateWithFlags(&x7, cudaStreamDefault);
  CUDA_CALL(x14);
  ncclResult_t x15 = ncclCommInitRank(&x6, x2, x5, x1);
  NCCLCHECK(x15);
  ncclDataType_t x16 = ncclFloat;
  NCCLCHECK(ncclGroupStart());
  if (x1 == 0) {
    int x17 = x2;
    int x18 = 0;
    while (x18 != x17) {
      int x19 = x18;
      NCCLCHECK(ncclRecv(x11[x19], 1024, x16, x19, x6, x7));
      x18 = x18 + 1;
    }
  }
  NCCLCHECK(ncclSend(x10, 1024, x16, 0, x6, x7));
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x7));
  int x20 = 0;
  if (x1 == 0) {
    float* x21 = (float*)malloc(1024 * sizeof(float));
    int x22 = x2;
    int x23 = 0;
    while (x23 != x22) {
      CUDA_CALL(cudaMemcpy(x21, x11[x23], (size_t)(1024 * sizeof(float)), cudaMemcpyDeviceToHost));
      int x24 = 0;
      while (x24 != 1024) {
        if (x21[x24] != 2) x20 = x20 + 1;
        x24 = x24 + 1;
      }
      x23 = x23 + 1;
    }
  }
  CUDA_CALL(cudaFree(x10));
  if (x1 == 0) {
    int x25 = x2;
    int x26 = 0;
    while (x26 != x25) {
      CUDA_CALL(cudaFree(x11[x26]));
      x26 = x26 + 1;
    }
  }
  NCCLCHECK(ncclCommDestroy(x6));
  MPICHECK(MPI_Finalize());
  if (x20 != 0) printf("[MPI Rank %d] Found %d errors.\n", x1, x20);
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
