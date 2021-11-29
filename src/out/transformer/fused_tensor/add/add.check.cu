/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x3(int x4, int x5, int x6) {
  int x7 = gridDim.x * blockDim.x;
  int x8 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x8 < x6) {
    x4[x8] = 1;
    x8 = x8 + x7;
  }
}
__global__ void x9(int x10, int x11, int x12) {
  int x13 = gridDim.x * blockDim.x;
  int x14 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x14 < x12) {
    x10[x14] = -1;
    x14 = x14 + x13;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x1, (lms.thirdparty.size_ttypeless$sizet)(10 * sizeof(Int))));
  int* x2 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x2, (lms.thirdparty.size_ttypeless$sizet)(10 * sizeof(Int))));
  show_tensor(x3(x1, 0, 0, dim3(0, 1, 1), dim3(0, 1, 1)));
  show_tensor(x9(x2, 0, 0, dim3(0, 1, 1), dim3(0, 1, 1)));
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
