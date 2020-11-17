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
  CudaError_T x6 = cudaStreamCreateWithFlags(&x5, cudaStreamDefault);
  CUDA_CALL(x6);
  ncclComm_t x7;
  ncclResult_t x8 = ncclCommInitRank(&x7, x2, x4, x1);
  NCCLCHECK(x8);
  float* x9 = (float*)malloc(0 * sizeof(float));
  CudaError_T x10 = cudaMalloc(&x9, (size_t)(33554432 * sizeof(float)));
  CUDA_CALL(x10);
  float* x11 = (float*)malloc(33554432 * sizeof(float));
  int x12 = 0;
  while (x12 != 33554432) {
    x11[x12] = 1.0;
    x12 = x12 + 1;
  }
  float** x13 = (float**)malloc(x2 * sizeof(float*));
  int x14 = x2;
  int x15 = 0;
  while (x15 != x14) {
    int x16 = x15;
    CUDA_CALL(cudaMalloc(&x13[x16], 33554432));
    CUDA_CALL(cudaMemcpy(x13[x16], x11, 33554432, cudaMemcpyHostToDevice));
    x15 = x15 + 1;
  }
  NCCLCHECK(ncclGroupStart());
  if (x1 == 0) {
    int x17 = x2;
    int x18 = 0;
    while (x18 != x17) {
      int x19 = x18;
      NCCLCHECK(ncclRecv(x13[x19], 33554432, ncclFloat, x19, x7, x5));
      x18 = x18 + 1;
    }
    NCCLCHECK(ncclSend(x9, 33554432, ncclFloat, 0, x7, x5));
  }
  NCCLCHECK(ncclGroupEnd());
  CUDA_CALL(cudaStreamSynchronize(x5));
  CUDA_CALL(cudaMemcpy(x11, x9, 33554432, cudaMemcpyDeviceToHost));
  int x20 = 0;
  while (x20 != 33554432) {
    if (x11[0] != 1) printf("error");
    x20 = x20 + 1;
  }
  CUDA_CALL(cudaFree(x9));
  CUDA_CALL(cudaFree(x13));
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
