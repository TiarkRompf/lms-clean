/*****************************************
Emitting C Generated Code
*******************************************/
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
  while (x4 < 6) {
    int x5 = x4;
    x3[x5] = x1[x5] + x2[x5];
    x4 = x4 + 1;
  }
  printf("%d ", {2, 3});
  int x6 = 0;
  while (x6 != 6) {
    printf("%d ", x3[x6]);
    x6 = x6 + 1;
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
