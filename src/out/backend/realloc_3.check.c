/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int* x1 = (int*)malloc(10L * sizeof(int));
  x1[0L] = 3;
  int* x2 = (int*)realloc(x1, 20L * sizeof(int));
  x2[4L] = 3;
  printf("%d\n", x2[3L]);
  free(x2);
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
