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
  int x1[6] = { 1, 2, 3, 4, 5, 6 };
  int x2[6] = { 6, 5, 4, 3, 2, 1 };
  int* x3 = (int*)malloc(6 * sizeof(int));
  int x4 = 0;
  while (x4 != 6) {
    int x5 = x4;
    x3[x5] = x1[x5] + x2[x5];
    x4 = x4 + 1;
  }
  int x6 = 0;
  while (x6 != 6) {
    printf("%d ", x3[x6]);
    x6 = x6 + 1;
  }
  int* x7 = (int*)malloc(6 * sizeof(int));
  int x8 = 0;
  while (x8 != 6) {
    int x9 = x8;
    x7[x9] = x1[x9] - x2[x9];
    x8 = x8 + 1;
  }
  int x10 = 0;
  while (x10 != 6) {
    printf("%d ", x7[x10]);
    x10 = x10 + 1;
  }
  int* x11 = (int*)malloc(6 * sizeof(int));
  int x12 = 0;
  while (x12 != 6) {
    int x13 = x12;
    x11[x13] = x1[x13] * x2[x13];
    x12 = x12 + 1;
  }
  int x14 = 0;
  while (x14 != 6) {
    printf("%d ", x11[x14]);
    x14 = x14 + 1;
  }
  int* x15 = (int*)malloc(6 * sizeof(int));
  int x16 = 0;
  while (x16 != 6) {
    int x17 = x16;
    x15[x17] = x1[x17] / x2[x17];
    x16 = x16 + 1;
  }
  int x18 = 0;
  while (x18 != 6) {
    printf("%d ", x15[x18]);
    x18 = x18 + 1;
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
