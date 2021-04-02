/*****************************************
Emitting C Generated Code
*******************************************/
#include <string.h>
#include <stdlib.h>
#include "cuda_header.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  // sanity check only, not runnable code
  __shared__ int x1[4];
  x1[0] = x0;
  printf("%d", x1[1]);
  printf("%d", x1[3]);
  __shared__ int x2[12];
  x2[0] = x0;
  printf("%d", x0);
  printf("%d", x2[1]);
  printf("%d", x2[2]);
  printf("%d", x2[3]);
  printf("%d", x2[4]);
  printf("%d", x2[5]);
  printf("%d", x2[6]);
  printf("%d", x2[7]);
  printf("%d", x2[8]);
  printf("%d", x2[9]);
  printf("%d", x2[10]);
  printf("%d", x2[11]);
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
