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
__global__ void x4(float* x5, float* x6, int* x7, float x8, int x9, int x10, int x11, int x12) {
  int x13 = blockIdx.x * blockIdx.y + threadIdx.x;
  int x14 = x13;
  int x15 = blockDim.x * gridDim.x;
  int x16 = x13 / x10;
  int x17 = x16;
  int x18 = x16 * x10;
  int x19 = x13 - x18;
  int x20 = x19 / x10;
  int x21 = x20;
  int x22 = x20 * x10;
  int x23 = x18 + x22 + (x19 - x22);
  while (x23 < x12) {
    x6[x23] = x7[x21 % x9 * x9 + x17 % x9] == 0 ? x8 : x5[x23];
    int x24 = x14 + x15;
    x14 = x24;
    int x25 = x24 / x10;
    x17 = x25;
    int x26 = x25 * x10;
    int x27 = x24 - x26;
    int x28 = x27 / x10;
    x21 = x28;
    int x29 = x28 * x10;
    x23 = x26 + x29 + (x27 - x29);
  }
}
__global__ void x30(float* x31, float* x32, int* x33, int x34, int x35, int x36, int x37) {
  int x38 = blockIdx.x * blockIdx.y + threadIdx.x;
  int x39 = x38;
  int x40 = blockDim.x * gridDim.x;
  int x41 = x38 / x35;
  int x42 = x41;
  int x43 = x41 * x35;
  int x44 = x38 - x43;
  int x45 = x44 / x35;
  int x46 = x45;
  int x47 = x45 * x35;
  int x48 = x43 + x47 + (x44 - x47);
  while (x48 < x37) {
    if (x33[x46 % x34 * x34 + x42 % x34] == 0) x32[x48] = x32[x48] + x31[x48];
    int x49 = x39 + x40;
    x39 = x49;
    int x50 = x49 / x35;
    x42 = x50;
    int x51 = x50 * x35;
    int x52 = x49 - x51;
    int x53 = x52 / x35;
    x46 = x53;
    int x54 = x53 * x35;
    x48 = x51 + x54 + (x52 - x54);
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(64 * sizeof(float));
  int* x2 = (int*)malloc(64 * sizeof(int));
  float* x3 = (float*)malloc(64 * sizeof(float));
  x4<<<dim3(1, 1, 1), dim3(1, 1, 1)>>>(x1, x3, x2, 1.0, 8, 1, 0, 64);
  x30<<<dim3(1, 1, 1), dim3(1, 1, 1)>>>(x1, x3, x2, 8, 1, 0, 64);
  int x55 = 0;
  while (x55 != 64) {
    printf("%d,", x3[64]);
    x55 = x55 + 1;
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
