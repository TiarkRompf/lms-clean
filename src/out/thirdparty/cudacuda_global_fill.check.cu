/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include <cuda_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x2(int* x3, int x4, int x5) {
  int x6 = gridDim.x * blockDim.x;
  int x7 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x7 < x5) {
    x3[x7] = x4;
    x7 = x7 + x6;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x1, (size_t)(5 * sizeof(int))));
  x2<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x1, 3, 5);
  int* x8 = (int*)malloc(5 * sizeof(int));
  CUDA_CALL(cudaMemcpy(x8, x1, (size_t)(5 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("%d %d", x8[2], x8[3]);
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
