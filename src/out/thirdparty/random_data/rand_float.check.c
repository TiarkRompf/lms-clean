/*****************************************
Emitting C Generated Code
*******************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  int x1 = 0;
  while (x1 != 10) {
    printf("%f ", (float)(rand() - RAND_MAX / 2) / (float)RAND_MAX);
    x1 = x1 + 1;
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
