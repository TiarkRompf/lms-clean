/*****************************************
Emitting C Generated Code
*******************************************/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stdlib.h>
#include <scanner_header.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
/**************** Snippet ****************/
void Snippet(int x0) {
  FILE* x1 = fopen("/scratch-ml00/wang603/lms-umbrella/lms-clean/src/test/scala/lms/thirdparty/test_binary", "w");
  fprintf(x1, "%d", 10);
  fclose(x1);
  FILE* x2 = fopen("/scratch-ml00/wang603/lms-umbrella/lms-clean/src/test/scala/lms/thirdparty/test_binary", "r");
  int x3 = 0;
  if (fscanf(x2, "%d", &x3) != 1) perror("Error reading file");
  printf("%d", x3);
  fclose(x2);
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
