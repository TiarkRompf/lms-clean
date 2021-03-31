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
__global__ void x4(float* x5, float* x6, int* x7, float x8, int x9, int x10, int x11, int x12, int x13) {
  int x14 = blockIdx.x * blockIdx.y + threadIdx.x;
  int x15 = x14;
  int x16 = blockDim.x * gridDim.x;
  int x17 = x14 / x11;
  int x18 = x17;
  int x19 = x17 * x11;
  int x20 = x14 - x19;
  int x21 = x20 / x12;
  int x22 = x21;
  int x23 = x21 * x12;
  int x24 = x19 + x23 + (x20 - x23);
  while (x24 < x13) {
    x6[x24] = x7[x22 % x10 * x9 + x18 % x9] == 0 ? x8 : x5[x24];
    int x25 = x15 + x16;
    x15 = x25;
    int x26 = x25 / x11;
    x18 = x26;
    int x27 = x26 * x11;
    int x28 = x25 - x27;
    int x29 = x28 / x12;
    x22 = x29;
    int x30 = x29 * x12;
    x24 = x27 + x30 + (x28 - x30);
  }
}
__global__ void x31(float* x32, float* x33, int* x34, int x35, int x36, int x37, int x38, int x39) {
  int x40 = blockIdx.x * blockIdx.y + threadIdx.x;
  int x41 = x40;
  int x42 = blockDim.x * gridDim.x;
  int x43 = x40 / x37;
  int x44 = x43;
  int x45 = x43 * x37;
  int x46 = x40 - x45;
  int x47 = x46 / x38;
  int x48 = x47;
  int x49 = x47 * x38;
  int x50 = x45 + x49 + (x46 - x49);
  while (x50 < x39) {
    if (x34[x48 % x36 * x35 + x44 % x35] == 0) x33[x50] = x33[x50] + x32[x50];
    int x51 = x41 + x42;
    x41 = x51;
    int x52 = x51 / x37;
    x44 = x52;
    int x53 = x52 * x37;
    int x54 = x51 - x53;
    int x55 = x54 / x38;
    x48 = x55;
    int x56 = x55 * x38;
    x50 = x53 + x56 + (x54 - x56);
  }
}
/**************** Snippet ****************/
void Snippet(int x0) {
  float* x1 = (float*)malloc(64 * sizeof(float));
  int* x2 = (int*)malloc(64 * sizeof(int));
  float* x3 = (float*)malloc(64 * sizeof(float));
  x4<<<dim3(1, 1, 1), dim3(1, 1, 1)>>>(x1, x3, x2, 1.0, 8, 8, 1, 1, 64);
  x31<<<dim3(1, 1, 1), dim3(1, 1, 1)>>>(x1, x3, x2, 8, 8, 1, 1, 64);
  int x57 = 0;
  while (x57 != 64) {
    printf("%d,", x3[64]);
    x57 = x57 + 1;
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
