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
  int x1 = open("/scratch-ml00/wang603/lms-umbrella/lms-clean/src/test/scala/lms/thirdparty/test_scanner.scala", 0);
  char* x2 = (char*)mmap(0, 50L, PROT_READ | PROT_WRITE, MAP_FILE | MAP_PRIVATE, x1, 0);
  int x3 = 0;
  while (x3 != 50) {
    printf("%c", x2[x3]);
    x3 = x3 + 1;
  }
  close(x1);
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
