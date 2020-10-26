/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1[5] = { 1, 2, 3, 4, 5 };
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(5 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(5 * sizeof(int)), cudaMemcpyHostToDevice));
  clipAt<<<28, 512>>>(x2, 2, 5);
  int* x3 = (int*)malloc(5 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x3, x2, (size_t)(5 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("%d, %d", x3[0], x3[4]);
  CUDA_CALL(cudaFree(x2));
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
