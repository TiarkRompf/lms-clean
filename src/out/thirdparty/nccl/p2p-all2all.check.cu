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
  int x3 = MPI_Comm_size(MPI_COMM_WORLD, &x2);
  MPICHECK(x3);
  ncclUniqueId x4;
  if (x1 == 0) NCCLCHECK(ncclGetUniqueId(&x4));
  MPICHECK(MPI_Bcast(&x4, NCCL_UNIQUE_ID_BYTES, MPI_BYTE, 0, MPI_COMM_WORLD));
  CUDA_CALL(cudaSetDevice(0));
  cudaStream_t x5;
  cudaError_t x6 = cudaStreamCreateWithFlags(&x5, cudaStreamDefault);
  CUDA_CALL(x6);
  ncclComm_t x7;
  ncclResult_t x8 = ncclCommInitRank(&x7, x2, x4, x1);
  NCCLCHECK(x8);
  float* x9 = (float*)malloc(33554432 * sizeof(float));
  int x10 = 0;
  while (x10 != 33554432) {
    x9[x10] = 1.0;
    x10 = x10 + 1;
  }
  float** x11 = (float**)malloc(x2 * sizeof(float*));
  int x12 = x2;
  int x13 = 0;
  while (x13 != x12) {
    int x14 = x13;
    CUDA_CALL(cudaMalloc(&x11[x14], 33554432));
    CUDA_CALL(cudaMemcpy(x11[x14], x9, 33554432, cudaMemcpyHostToDevice));
    x13 = x13 + 1;
  }
  float** x15 = (float**)malloc(x2 * sizeof(float*));
  int x16 = x2;
  int x17 = 0;
  while (x17 != x16) {
    CUDA_CALL(cudaMalloc(&x15[x17], 33554432));
    x17 = x17 + 1;
  }
  NCCLCHECK(ncclGroupStart());
  if (x1 == 0) {
    int x18 = x2;
    int x19 = 0;
    while (x19 != x18) {
      int x20 = x19;
      NCCLCHECK(ncclSend(x11[x20], 33554432, ncclFloat, x20, x7, x5));
      NCCLCHECK(ncclRecv(x15[x20], 33554432, ncclFloat, x20, x7, x5));
      x19 = x19 + 1;
    }
  }
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x5));
  int x21 = 0;
  while (x21 != 33554432) {
    CUDA_CALL(cudaMemcpy(x9, x15[x21], 33554432, cudaMemcpyDeviceToHost));
    if (x9[0] != 1) printf("error");
    x21 = x21 + 1;
  }
  CUDA_CALL(cudaFree(x11));
  CUDA_CALL(cudaFree(x15));
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
