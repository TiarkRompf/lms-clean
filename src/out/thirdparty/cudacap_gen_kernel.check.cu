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
__global__ void x3(float* x4, float x5, int x6) {
  int x7 = gridDim.x * blockDim.x;
  int x8 = threadIdx.x + blockIdx.x * blockDim.x;
  float x9 = -x5;
  while (x8 < x6) {
    int x10 = x8;
    if (x4[x10] > x5) x4[x10] = x5;
    if (x4[x10] < x9) x4[x10] = x9;
    x8 = x8 + x7;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float x1[5] = { 1.0, 2.0, 3.0, 4.0, 5.0 };
  float* x2 = (float*)malloc(0 * sizeof(float));
  CUDA_CALL(cudaMalloc(&x2, (size_t)(5 * sizeof(int))));
  CUDA_CALL(cudaMemcpy(x2, x1, (size_t)(5 * sizeof(int)), cudaMemcpyHostToDevice));
  x3<<<dim3(28, 1, 1), dim3(512, 1, 1)>>>(x2, 2.0, 5);
  float* x11 = (float*)malloc(5 * sizeof(float));
  CUDA_CALL(cudaMemcpy(x11, x2, (size_t)(5 * sizeof(int)), cudaMemcpyDeviceToHost));
  printf("%d, %d", x11[0], x11[4]);
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
