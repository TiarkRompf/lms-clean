/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int32_t x0) {
  int32_t* x1 = (int32_t*)malloc(10 * sizeof(int32_t));
  int32_t* x2 = (int32_t*)malloc(10 * sizeof(int32_t));
  int32_t x3 = 0;
  while (x3 < 10) {
    x1[x3] = x3;
    x3 = x3 + 1;
  }
  memcpy(x2 + 0, x1, 10);
  x3 = 0;
  while (x3 < 10) {
    printf("%d\n", x2[x3]);
    x3 = x3 + 1;
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
