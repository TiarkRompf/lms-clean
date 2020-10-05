/*****************************************
Emitting C Generated Code
*******************************************/
#include <nccl_header.h>
#include <string.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <mpi_header.h>
#include <nccl.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  ncclUniqueId x1;
  int x2 = 0;
  int x3 = 0;
  MPICHECK(MPI_Init(NULL, NULL));
  int x4 = MPI_Comm_rank(MPI_COMM_WORLD, &x3);
  MPICHECK(x4);
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x2));
  if (x0 < x2) {
    if (x3 == 0) printf("Usage : %s <GPU list per rank\n", x0);
    fflush(stdout); fflush(stderr); exit(1);
  }
  CUDA_CALL(cudaSetDevice(x3));
  MPICHECK(MPI_Barrier(MPI_COMM_WORLD));
  ncclComm_t x5;
  NCCLCHECK(ncclGetUniqueId(&x1));
  MPICHECK(MPI_Bcast(&x1, NCCL_UNIQUE_ID_BYTES, MPI_CHAR, 0, MPI_COMM_WORLD));
  NCCLCHECK(ncclCommInitRank(&x5, x2, x1, x3));
  cudaStream_t x6;
  CUDA_CALL(cudaStreamCreateWithFlags(&x6, cudaStreamNonBlocking));
  int* x7 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x7, (size_t)(128 * sizeof(int))));
  int* x8 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x8, (size_t)(128 * sizeof(int))));
  int* x9 = (int*)malloc(128 * sizeof(int));
  int x10 = 0;
  while (x10 != 128) {
    x9[x10] = x3 + 1;
    x10 = x10 + 1;
  }
  CUDA_CALL(cudaMemcpy(x7, x9, 128, cudaMemcpyHostToDevice));
  int x11 = x2 * (x2 + 1) / 2;
  int x12 = 0;
  int x13 = 0;
  while (x13 != 1) {
    NCCLCHECK(ncclAllReduce(x7, x8, 128, ncclInt, ncclSum, x5, x6));
    x13 = x13 + 1;
  }
  CUDA_CALL(cudaStreamSynchronize(x6));
  CUDA_CALL(cudaMemcpy(x9, x8, 128, cudaMemcpyDeviceToHost));
  int x14 = 0;
  while (x14 != 128) {
    if (x9[x14] != x11) {
      x12 = x12 + 1;
      printf("error");
    }
    x14 = x14 + 1;
  }
  CUDA_CALL(cudaFree(x8));
  CUDA_CALL(cudaFree(x7));
  MPICHECK(MPI_Allreduce(MPI_IN_PLACE, &x12, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD));
  if (x3 == 0) if (x12 > 0) printf("%d errors. Test FAILED.\n", x12);
  else printf("test PASSED.\n");
  MPICHECK(MPI_Finalize());
  NCCLCHECK(ncclCommDestroy(x5));
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
