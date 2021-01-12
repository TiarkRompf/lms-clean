/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x1, (size_t)(5 * sizeof(int))));
  arrayFill<<<28, 512>>>(x1, 8, 5);
  int* x2 = (int*)malloc(5 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(5 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("%d %d", x2[1], x2[4]);
  CUDA_CALL(cudaFree(x1));
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
