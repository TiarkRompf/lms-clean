/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  printf("%d ", {2, 2, 2});
  int x2 = 0;
  while (x2 != 8) {
    printf("%d ", x1[x2]);
    x2 = x2 + 1;
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
