/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x2(int x3, int x4, int x5) {
  int x6 = gridDim.x * blockDim.x;
  int x7 = threadIdx.x + blockIdx.x * blockDim.x;
  while (x7 < x5) {
    int x8 = x7;
    x3[x7] = x8 < 5 ? x3[x8] + 1 : x3[x8];
    x7 = x7 + x6;
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(0 * sizeof(int));
  CUDA_CALL(cudaMalloc(&x1, (lms.thirdparty.size_ttypeless$sizet)(10 * sizeof(Int))));
  show_tensor(x2(x1, 0, 20, dim3(0, 1, 1), dim3(0, 1, 1)));
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
