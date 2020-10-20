/*****************************************
Emitting C Generated Code
*******************************************/
#include <cblas.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  float x1[3] = { 1.0, 2.0, 3.0 };
  float x2[6] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  float x3[6] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  float* x4 = (float*)malloc(1 * sizeof(float));
  int x5 = 0;
  while (x5 != 3) {
    int x6 = x5;
    x4[0] = x4[0] + x1[x6] * x1[x6];
    x5 = x5 + 1;
  }
  int x7 = 0;
  while (x7 != 1) {
    printf("%f ", x4[x7]);
    x7 = x7 + 1;
  }
  float* x8 = (float*)malloc(2 * sizeof(float));
  cblas_sgemv(CblasRowMajor, CblasNoTrans, 2, 3, 1.0, x2, 3, x1, 1, 0.0, x8, 1);
  int x9 = 0;
  while (x9 != 2) {
    printf("%f ", x8[x9]);
    x9 = x9 + 1;
  }
  float* x10 = (float*)malloc(4 * sizeof(float));
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, 2, 2, 3, 1.0, x2, 3, x3, 2, 0.0, x10, 2);
  int x11 = 0;
  while (x11 != 4) {
    printf("%f ", x10[x11]);
    x11 = x11 + 1;
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
