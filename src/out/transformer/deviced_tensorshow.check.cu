/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <cblas.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x1, (size_t)(6 * sizeof(float))));
  float* x2 = (float*)malloc(6 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(6 * sizeof(float)), cudaMemcpyDeviceToHost));
  int x3 = 0;
  while (x3 != 6) {
    printf("%f ", x2[x3]);
    x3 = x3 + 1;
  }
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
