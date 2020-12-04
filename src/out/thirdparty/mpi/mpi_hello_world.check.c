/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "mpi_header.h"
/**************** Snippet ****************/
void Snippet(int x0) {
  MPICHECK(MPI_Init(NULL, NULL));
  int x1 = 0;
  MPICHECK(MPI_Comm_size(MPI_COMM_WORLD, &x1));
  int x2 = 0;
  MPICHECK(MPI_Comm_rank(MPI_COMM_WORLD, &x2));
  char* x3 = (char*)malloc(MPI_MAX_PROCESSOR_NAME * sizeof(char));
  int x4 = 0;
  MPICHECK(MPI_Get_processor_name(x3, &x4));
  printf("Hellw world from processor %s, rank %d out of %d processors\n", x3, x2, x1);
  MPICHECK(MPI_Finalize());
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
