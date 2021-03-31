/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/************* Functions **************/
__global__ void x2(float* x3, float* x4, int* x5, int x6, int x7, int x8, int x9, int x10) {
  int x11 = blockIdx.x * blockIdx.y + threadIdx.x;
  int x12 = x11;
  int x13 = blockDim.x * gridDim.x;
  int x14 = x11 / x8;
  int x15 = x14;
  int x16 = x14 * x8;
  int x17 = x11 - x16;
  int x18 = x17 / x8;
  int x19 = x18;
  int x20 = x18 * x8;
  int x21 = x16 + x20 + (x17 - x20);
  while (x21 < x10) {
    if (x5[x19 % x7 * x6 + x15 % x6] == 0) x4[x21] = x4[x21] + x3[x21];
    int x22 = x12 + x13;
    x12 = x22;
    int x23 = x22 / x8;
    x15 = x23;
    int x24 = x23 * x8;
    int x25 = x22 - x24;
    int x26 = x25 / x8;
    x19 = x26;
    int x27 = x26 * x8;
    x21 = x24 + x27 + (x25 - x27);
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(64 * sizeof(float));
  x2<<<dim3(1, 1, 1), dim3(1, 1, 1)>>>((float*)malloc(64 * sizeof(float)), x1, (int*)malloc(64 * sizeof(int)), 8, 8, 1, 0, 64);
  int x28 = 0;
  while (x28 != 64) {
    printf("%d,", x1[64]);
    x28 = x28 + 1;
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
