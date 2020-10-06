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
  int* x1 = (int*)malloc(6 * sizeof(int));
  int x2 = 0;
  while (x2 != 6) {
    int x3 = x2;
    x1[x3] = x3;
    x2 = x2 + 1;
  }
  int x4 = 0;
  while (x4 != 6) {
    printf("%d ", x1[x4]);
    x4 = x4 + 1;
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
